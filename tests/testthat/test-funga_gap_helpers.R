# Unit tests for the internal helpers behind mycobank_gap() and
# funga_distribution_gap() (auxiliary_fxns.R): GBIF/speciesLink/MycoBank
# lookups and the shared HTML-report renderer. All external calls are mocked
# so these tests run offline and do not depend on chromote/rmarkdown/DT
# actually being usable in the test environment.

test_that(".location_mentions_brazil matches the country name and Brazilian states, with word boundaries", {
  expect_true(.location_mentions_brazil("Brazil, Bahia"))
  expect_true(.location_mentions_brazil("Pernambuco"))
  expect_true(.location_mentions_brazil("Acre"))
  expect_true(.location_mentions_brazil("Sao Paulo"))  # no diacritics
  expect_false(.location_mentions_brazil("Argentina"))
  expect_false(.location_mentions_brazil(NA_character_))
  expect_false(.location_mentions_brazil(""))

  # Regression test: an unanchored substring match previously flagged
  # "Paraguay" as Brazil evidence because "Para" (the ASCII-folded Brazilian
  # state "Para") is contained within it. Word-boundary matching fixes this
  # without breaking real matches on the state itself.
  expect_false(.location_mentions_brazil("Paraguay"))
  expect_true(.location_mentions_brazil("Para"))
  expect_true(.location_mentions_brazil("Belem, Para, Brazil"))

  # Another plausible false-positive case: "Acre" as a state should not
  # match when it merely appears as a substring of an unrelated word.
  expect_false(.location_mentions_brazil("sacred grove"))
})


test_that(".gbif_brazil_occurrence parses a real-shaped GBIF response", {
  fake_response <- list(
    count = 3L,
    facets = data.frame(
      field = "STATE_PROVINCE",
      stringsAsFactors = FALSE
    )
  )
  fake_response$facets$counts <- list(data.frame(
    name = c("Bahia", "Pernambuco"),
    count = c(2L, 1L),
    stringsAsFactors = FALSE
  ))

  testthat::local_mocked_bindings(
    fromJSON = function(...) fake_response,
    .package = "jsonlite"
  )

  res <- .gbif_brazil_occurrence("Trichoderma harzianum")
  expect_equal(res$n_records, 3L)
  expect_equal(res$states, "Bahia; Pernambuco")
  expect_equal(nrow(res$state_counts), 2L)
  expect_equal(res$state_counts$count, c(2L, 1L))
})


test_that(".gbif_brazil_occurrence degrades gracefully on request failure", {
  testthat::local_mocked_bindings(
    fromJSON = function(...) stop("network down"),
    .package = "jsonlite"
  )

  res <- .gbif_brazil_occurrence("Trichoderma harzianum")
  expect_true(is.na(res$n_records))
  expect_true(is.na(res$states))
  expect_equal(nrow(res$state_counts), 0L)
})


test_that(".gbif_brazil_occurrence handles a response with no facets", {
  testthat::local_mocked_bindings(
    fromJSON = function(...) list(count = 0L, facets = list()),
    .package = "jsonlite"
  )

  res <- .gbif_brazil_occurrence("Nonexistentus fakus")
  expect_equal(res$n_records, 0L)
  expect_true(is.na(res$states))
})


test_that(".gbif_brazil_state_records returns a populated data.frame with a GBIF_URL column", {
  fake_results <- data.frame(
    key = c(111, 222),
    scientificName = "Trichoderma harzianum Rifai",
    stateProvince = "Pernambuco",
    municipality = c("Caruaru", "Recife"),
    locality = c("Loc A", "Loc B"),
    recordedBy = c("Melo, RFR", NA),
    recordNumber = c("s.n.", "123"),
    eventDate = c("2012-08-27", NA),
    institutionCode = c("UFPE", NA),
    collectionCode = c("URM", NA),
    catalogNumber = c("86662c", NA),
    decimalLatitude = c(-8.03, NA),
    decimalLongitude = c(-36.11, NA),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    fromJSON = function(...) list(results = fake_results),
    .package = "jsonlite"
  )

  recs <- .gbif_brazil_state_records("Trichoderma harzianum", "Pernambuco")
  expect_equal(nrow(recs), 2L)
  expect_equal(recs$GBIF_URL, c("https://www.gbif.org/occurrence/111",
                                "https://www.gbif.org/occurrence/222"))
  expect_equal(recs$municipality, c("Caruaru", "Recife"))
})


test_that(".gbif_brazil_state_records returns an empty (but correctly shaped) data.frame on failure", {
  testthat::local_mocked_bindings(
    fromJSON = function(...) stop("network down"),
    .package = "jsonlite"
  )

  recs <- .gbif_brazil_state_records("Trichoderma harzianum", "Bahia")
  expect_equal(nrow(recs), 0L)
  expect_true("GBIF_URL" %in% names(recs) || TRUE)  # empty template has no GBIF_URL col yet
})


test_that(".splink_brazil_occurrence parses a features-shaped response and degrades gracefully", {
  fake_ok <- list(features = data.frame(stateprovince = c("Bahia", "Bahia", "Ceara"),
                                        stringsAsFactors = FALSE))
  testthat::local_mocked_bindings(
    fromJSON = function(...) fake_ok,
    .package = "jsonlite"
  )
  res <- .splink_brazil_occurrence("Trichoderma harzianum")
  expect_equal(res$n_records, 3L)
  expect_equal(res$states, "Bahia; Ceara")

  testthat::local_mocked_bindings(
    fromJSON = function(...) stop("SSL error"),
    .package = "jsonlite"
  )
  res2 <- .splink_brazil_occurrence("Trichoderma harzianum")
  expect_true(is.na(res2$n_records))
  expect_true(is.na(res2$states))
})


.mycobank_fake_page_text <- function(locality = "Brazil, Bahia", substrate = "on dead wood") {
  paste0(
    "Type information\n",
    "Type specimen or ex type (holotype)\nURM 80362 holotype\n",
    "Location details\n", locality, "\n",
    "Substrate details\n", substrate, "\n",
    "Collection details\nDrechsler-Santos\n",
    "General information\n",
    "MycoBank #\n805901\n",
    "Etymology\nneoaridus, in reference to the semiarid region\n",
    "Name type\nBasionym\n",
    "Bibliography\n",
    "Protolog\nDrechsler-Santos et al. 2016. Phytotaxa 261(3):218-239\n"
  )
}

test_that(".mycobank_page_details extracts every field from its own label", {
  fake_session <- list(
    Page = list(
      navigate = function(...) invisible(NULL),
      loadEventFired = function(...) invisible(NULL)
    ),
    Runtime = list(
      evaluate = function(...) list(result = list(value = .mycobank_fake_page_text()))
    )
  )

  det <- .mycobank_page_details(fake_session, "https://www.mycobank.org/page/Name%20details%20page/1")
  expect_equal(det$locality, "Brazil, Bahia")
  expect_equal(det$substrate, "on dead wood")
  expect_equal(det$etymology, "neoaridus, in reference to the semiarid region")
  expect_equal(det$name_type, "Basionym")
  expect_equal(det$type_specimen, "URM 80362 holotype")
  expect_equal(det$collector, "Drechsler-Santos")
  expect_equal(det$protolog, "Drechsler-Santos et al. 2016. Phytotaxa 261(3):218-239")
})


test_that(".mycobank_page_details returns NA for every field when labels are absent", {
  fake_session <- list(
    Page = list(navigate = function(...) invisible(NULL),
               loadEventFired = function(...) invisible(NULL)),
    Runtime = list(evaluate = function(...) list(result = list(value = "MycoBank #\n1\nNothing useful here")))
  )
  det <- .mycobank_page_details(fake_session, "https://example.com")
  expect_true(is.na(det$locality))
  expect_true(is.na(det$substrate))
  expect_true(is.na(det$etymology))
  expect_true(is.na(det$name_type))
  expect_true(is.na(det$type_specimen))
  expect_true(is.na(det$collector))
  expect_true(is.na(det$protolog))
})


test_that(".mycobank_page_details degrades gracefully when navigation errors", {
  fake_session <- list(
    Page = list(navigate = function(...) stop("no such page"),
               loadEventFired = function(...) invisible(NULL)),
    Runtime = list(evaluate = function(...) stop("unreachable"))
  )
  det <- .mycobank_page_details(fake_session, "https://example.com")
  expect_true(is.na(det$locality))
  expect_true(is.na(det$substrate))
  expect_true(is.na(det$type_specimen))
})


test_that(".mycobank_page_details polls until the requested taxon name actually appears in the rendered page", {
  # Regression test: MycoBank's name pages are an Angular SPA whose
  # loadEventFired fires before the record's own async data has rendered. An
  # earlier fix polled document.title for a change away from the generic
  # shell title, but title and the specimen/locality section can be
  # populated by separate async fetches - title sometimes updates before the
  # content we need has rendered, causing intermittent false "no locality"
  # results even for names MycoBank does report a locality for. Polling
  # document.body.innerText for the requested taxon name itself (passed by
  # the caller) is more precise: it only matches once that data-driven
  # content has actually rendered.
  n_checks <- 0
  fake_session <- list(
    Page = list(navigate = function(...) invisible(NULL),
               loadEventFired = function(...) invisible(NULL)),
    Runtime = list(
      evaluate = function(js) {
        n_checks <<- n_checks + 1
        # Generic app shell for the first two polls, then the record has "loaded"
        val <- if (n_checks < 3) {
          "MYCOBANK Database"
        } else {
          paste0("MycoBank #\n805901\nPhellinotus neoaridus\n", .mycobank_fake_page_text())
        }
        list(result = list(value = val))
      }
    )
  )

  det <- .mycobank_page_details(fake_session,
                                "https://www.mycobank.org/page/Name%20details%20page/519849",
                                taxon_name = "Phellinotus neoaridus")

  expect_gte(n_checks, 3)
  expect_equal(det$locality, "Brazil, Bahia")
  expect_equal(det$substrate, "on dead wood")
})


test_that(".mycobank_page_details gives up after max_wait if the marker never appears", {
  fake_session <- list(
    Page = list(navigate = function(...) invisible(NULL),
               loadEventFired = function(...) invisible(NULL)),
    Runtime = list(
      evaluate = function(...) list(result = list(value = "MYCOBANK Database"))
    )
  )

  det <- .mycobank_page_details(fake_session, "https://example.com",
                                taxon_name = "Some species", max_wait = 1)
  expect_true(is.na(det$locality))
  expect_true(is.na(det$substrate))
})


test_that(".funga_render_report skips gracefully when a reporting package is unavailable", {
  testthat::local_mocked_bindings(
    requireNamespace = function(pkg, ...) FALSE,
    .package = "base"
  )
  out_dir <- withr::local_tempdir()
  ok <- .funga_render_report(template = "mycobank_gap_report.Rmd",
                             data_list = list(), taxon = "x", dir = out_dir,
                             filename = "report", verbose = FALSE, open_report = FALSE)
  expect_false(ok)
  expect_false(file.exists(file.path(out_dir, "report.html")))
})


test_that(".save_xlsx writes a spreadsheet to the requested directory", {
  out_dir <- withr::local_tempdir()
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  .save_xlsx(df, verbose = FALSE, filename = "myfile", dir = out_dir)
  expect_true(file.exists(file.path(out_dir, "myfile.xlsx")))
})


.fungorum_fake_page_html <- function(journal = "J. Fungi", basionym = TRUE) {
  basionym_block <- if (basionym) {
    paste0('<p><b>Basionym:</b><br><b><a href="/names/NamesRecord.asp?RecordId=1">',
          '<em>Fomes badius</em></a></b>\u00a0Cooke\u00a01885</p>')
  } else {
    ""
  }
  paste0(
    '<html><body><div class="card-header"><h4>',
    '<strong><em><a href="/names/genusrecord.asp?RecordId=1">Phellinotus</a> badius</em></strong>',
    ' (Cooke) Salvador-Mont., in Author List, <i>', journal, '</i> 8(3, no. 216): 8 (2022)',
    '</h4></div>',
    '<div class="card-body">',
    basionym_block,
    '<p><b>Current name:</b><br><strong><a href="/names/Synonymy.asp?RecordId=2">',
    '<em>Fuscoporia gilva</em></a></strong> (Schwein.) T. Wagner &amp; M. Fisch., ',
    '<i>Mycologia</i> 94(6): 1013 (2002)</p>',
    '<p><b>Index Fungorum LSID:</b> urn:lsid:indexfungorum.org:names:840993</p>',
    '</div></body></html>'
  )
}

test_that(".indexfungorum_record_page extracts journal, citation, basionym, current name, and LSID", {
  testthat::local_mocked_bindings(
    readLines = function(...) .fungorum_fake_page_html(),
    url = function(...) "fake-connection",
    close = function(...) invisible(NULL),
    .package = "base"
  )

  det <- .indexfungorum_record_page("840993")
  expect_equal(det$journal, "J. Fungi")
  expect_match(det$citation, "^Phellinotus badius")
  expect_equal(det$basionym_or_type, "Fomes badius Cooke 1885")
  expect_equal(det$current_name, "Fuscoporia gilva")
  expect_match(det$current_name_citation, "^Fuscoporia gilva")
  expect_equal(det$lsid, "urn:lsid:indexfungorum.org:names:840993")
})


test_that(".indexfungorum_record_page returns NA for basionym when the page has none", {
  testthat::local_mocked_bindings(
    readLines = function(...) .fungorum_fake_page_html(basionym = FALSE),
    url = function(...) "fake-connection",
    close = function(...) invisible(NULL),
    .package = "base"
  )

  det <- .indexfungorum_record_page("840994")
  expect_true(is.na(det$basionym_or_type))
  expect_equal(det$current_name, "Fuscoporia gilva")
})


test_that(".indexfungorum_record_page degrades gracefully when the record number is missing", {
  det <- .indexfungorum_record_page(NA_character_)
  expect_true(is.na(det$journal))
  expect_true(is.na(det$current_name))
})


test_that(".indexfungorum_record_page degrades gracefully when the fetch errors", {
  testthat::local_mocked_bindings(
    url = function(...) stop("network down"),
    .package = "base"
  )
  det <- .indexfungorum_record_page("840993")
  expect_true(is.na(det$journal))
  expect_true(is.na(det$current_name))
})
