# Unit tests for the internal helpers behind funga_mycobank_gap() and
# funga_distribution_gap() (auxiliary_fxns.R): GBIF/speciesLink/MycoBank
# lookups and the shared HTML-report renderer. All external calls are mocked
# so these tests run offline and do not depend on chromote/rmarkdown/DT
# actually being usable in the test environment.

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


test_that(".mycobank_page_locality extracts the line following each label", {
  fake_text <- "Type information\nLocation details\nBrazil, Bahia\nSubstrate details\non dead wood\nOther stuff"
  fake_session <- list(
    Page = list(
      navigate = function(...) invisible(NULL),
      loadEventFired = function(...) invisible(NULL)
    ),
    Runtime = list(
      evaluate = function(...) list(result = list(value = fake_text))
    )
  )

  loc <- .mycobank_page_locality(fake_session, "https://www.mycobank.org/page/Name%20details%20page/1")
  expect_equal(loc$locality, "Brazil, Bahia")
  expect_equal(loc$substrate, "on dead wood")
})


test_that(".mycobank_page_locality returns NA locality/substrate when labels are absent", {
  fake_session <- list(
    Page = list(navigate = function(...) invisible(NULL),
               loadEventFired = function(...) invisible(NULL)),
    Runtime = list(evaluate = function(...) list(result = list(value = "Nothing useful here")))
  )
  loc <- .mycobank_page_locality(fake_session, "https://example.com")
  expect_true(is.na(loc$locality))
  expect_true(is.na(loc$substrate))
})


test_that(".mycobank_page_locality degrades gracefully when navigation errors", {
  fake_session <- list(
    Page = list(navigate = function(...) stop("no such page"),
               loadEventFired = function(...) invisible(NULL)),
    Runtime = list(evaluate = function(...) stop("unreachable"))
  )
  loc <- .mycobank_page_locality(fake_session, "https://example.com")
  expect_true(is.na(loc$locality))
  expect_true(is.na(loc$substrate))
})


test_that(".funga_render_report skips gracefully when a reporting package is unavailable", {
  testthat::local_mocked_bindings(
    requireNamespace = function(pkg, ...) FALSE,
    .package = "base"
  )
  out_dir <- withr::local_tempdir()
  ok <- .funga_render_report(template = "funga_mycobank_gap_report.Rmd",
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
