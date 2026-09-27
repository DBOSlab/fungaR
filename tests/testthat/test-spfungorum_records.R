.mock_fungorum_search <- function(fixture, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .indexfungorum_name_search = function(taxon, rank, max_number) fixture,
    .package = "fungaR",
    .env = env
  )
  # Keep tests offline/deterministic by default: no per-record page fetched
  # (returns all-NA), so existing expectations about counts/filtering are
  # unaffected by the fetch_details = TRUE default. Tests targeting
  # fetch_details itself install their own mock instead.
  testthat::local_mocked_bindings(
    .indexfungorum_record_page = function(record_number) {
      list(journal = NA_character_, citation = NA_character_,
          basionym_or_type = NA_character_, current_name_citation = NA_character_,
          current_name = NA_character_, lsid = NA_character_)
    },
    .package = "fungaR",
    .env = env
  )
}

.fungorum_fixture <- function() {
  data.frame(
    Taxon_name = c("Phellinotus badius", "Phellinotus magnoporatus",
                  "Phellinotus neoaridus", "Phellinotus piptadeniae"),
    Authors = c("(Cooke) Salvador-Mont.", "Salvador-Mont.", "Drechsler-Santos & Robledo",
               "(Teixeira) Drechsler-Santos"),
    SpecificEpithet = c("badius", "magnoporatus", "neoaridus", "piptadeniae"),
    Year = c("2022", "2022", "2016", "2016"),
    Rank = c("sp.", "sp.", "sp.", "sp."),
    Name_status = c("Legitimate", "Legitimate", "Legitimate", "Legitimate"),
    Current_name = c("Phellinotus badius", "Phellinotus magnoporatus",
                     "Fomitiporella neoarida", "Phellinotus piptadeniae"),
    Location = c(NA_character_, "Peru", "Pernambuco", NA_character_),
    Host = c(NA_character_, "on living tree of Ocotea aurantiodora",
            "on Caesalpinia", NA_character_),
    Volume = c("8", "8", "261", "261"),
    Part = c("3, no. 216", "3, no. 216", "3", "3"),
    Page = c("8", "11", "224", "224"),
    PublishingAuthors = rep("Salvador-Montoya et al.", 4),
    Fungorum_Number = c("840993", "840994", "805901", "805902"),
    stringsAsFactors = FALSE
  )
}


test_that("spfungorum_records filters to only Brazil-evidence records by default", {
  .mock_fungorum_search(.fungorum_fixture())

  result <- spfungorum_records(taxon = "Phellinotus", rank = "genus",
                                   save = FALSE, html_report = FALSE, verbose = FALSE)

  expect_equal(nrow(result), 1)
  expect_equal(result$Taxon_name, "Phellinotus neoaridus")
  expect_true(result$Fungorum_Brazil_Evidence)
  expect_equal(result$Fungorum_URL,
              "https://www.speciesfungorum.org/names/NamesRecord.asp?RecordID=805901")
})


test_that("spfungorum_records recognizes a Brazilian state name alone (not just 'Brazil')", {
  # Regression check: Index Fungorum's LOCATION field very often names a
  # state directly (e.g. "Pernambuco") rather than the country itself.
  .mock_fungorum_search(.fungorum_fixture())

  result <- spfungorum_records(taxon = "Phellinotus", rank = "genus",
                                   require_brazil_evidence = FALSE,
                                   save = FALSE, html_report = FALSE, verbose = FALSE)

  neoaridus_row <- result[result$Taxon_name == "Phellinotus neoaridus", ]
  expect_true(neoaridus_row$Fungorum_Brazil_Evidence)

  peru_row <- result[result$Taxon_name == "Phellinotus magnoporatus", ]
  expect_false(peru_row$Fungorum_Brazil_Evidence)
})


test_that("spfungorum_records returns unfiltered records when require_brazil_evidence = FALSE", {
  .mock_fungorum_search(.fungorum_fixture())

  result <- spfungorum_records(taxon = "Phellinotus", rank = "genus",
                                   require_brazil_evidence = FALSE,
                                   save = FALSE, html_report = FALSE, verbose = FALSE)

  expect_equal(nrow(result), 4)
})


test_that("spfungorum_records returns an empty data.frame (not an error) with no matches", {
  .mock_fungorum_search(data.frame(Taxon_name = character(0), Authors = character(0),
                                   SpecificEpithet = character(0), Year = character(0),
                                   Rank = character(0), Name_status = character(0),
                                   Current_name = character(0), Location = character(0),
                                   Host = character(0), Volume = character(0),
                                   Part = character(0), Page = character(0),
                                   PublishingAuthors = character(0),
                                   Fungorum_Number = character(0), stringsAsFactors = FALSE))

  result <- spfungorum_records(taxon = "Nonexistentus", rank = "genus",
                                   save = FALSE, html_report = FALSE, verbose = FALSE)

  expect_equal(nrow(result), 0)
})


test_that("spfungorum_records prefers the freshly-scraped current name over the (possibly stale) bulk search value", {
  # Regression check: the bulk NameSearch response's Current_name is a cached
  # value on the *original* name's own row and can go stale relative to the
  # current name's own record - fetch_details = TRUE (the default) should
  # overwrite it with the value freshly scraped from this record's own page.
  .mock_fungorum_search(.fungorum_fixture())
  testthat::local_mocked_bindings(
    .indexfungorum_record_page = function(record_number) {
      if (identical(record_number, "840993")) {
        list(journal = "J. Fungi", citation = "Phellinotus badius (Cooke) ...",
            basionym_or_type = NA_character_,
            current_name_citation = "Fuscoporia gilva (Schwein.) T. Wagner & M. Fisch., J. Fungi 94(6): 1013 (2002)",
            current_name = "Fuscoporia gilva", lsid = "urn:lsid:indexfungorum.org:names:840993")
      } else {
        list(journal = NA_character_, citation = NA_character_,
            basionym_or_type = NA_character_, current_name_citation = NA_character_,
            current_name = NA_character_, lsid = NA_character_)
      }
    },
    .package = "fungaR"
  )

  result <- spfungorum_records(taxon = "Phellinotus", rank = "genus",
                               require_brazil_evidence = FALSE,
                               save = FALSE, html_report = FALSE, verbose = FALSE)

  badius_row <- result[result$Taxon_name == "Phellinotus badius", ]
  expect_equal(badius_row$Current_name, "Fuscoporia gilva")
  expect_equal(badius_row$Fungorum_Journal, "J. Fungi")
  expect_equal(badius_row$Fungorum_LSID, "urn:lsid:indexfungorum.org:names:840993")

  # Untouched candidates keep their original (bulk-response) Current_name.
  magnoporatus_row <- result[result$Taxon_name == "Phellinotus magnoporatus", ]
  expect_equal(magnoporatus_row$Current_name, "Phellinotus magnoporatus")
})


test_that("spfungorum_records skips the per-record detail fetch when fetch_details = FALSE", {
  .mock_fungorum_search(.fungorum_fixture())
  n_calls <- 0
  testthat::local_mocked_bindings(
    .indexfungorum_record_page = function(record_number) {
      n_calls <<- n_calls + 1
      list(journal = NA_character_, citation = NA_character_,
          basionym_or_type = NA_character_, current_name_citation = NA_character_,
          current_name = NA_character_, lsid = NA_character_)
    },
    .package = "fungaR"
  )

  result <- spfungorum_records(taxon = "Phellinotus", rank = "genus",
                               require_brazil_evidence = FALSE, fetch_details = FALSE,
                               save = FALSE, html_report = FALSE, verbose = FALSE)

  expect_equal(n_calls, 0)
  expect_false("Fungorum_Journal" %in% names(result))
})


test_that("spfungorum_records requires a single character taxon", {
  expect_error(spfungorum_records(taxon = NULL), "single character string")
  expect_error(spfungorum_records(taxon = c("A", "B")), "single character string")
})


test_that("spfungorum_records writes a real .xlsx file when save = TRUE", {
  .mock_fungorum_search(.fungorum_fixture())
  out_dir <- withr::local_tempdir()

  result <- spfungorum_records(taxon = "Phellinotus", rank = "genus",
                                   save = TRUE, html_report = FALSE,
                                   dir = out_dir, filename = "fungorum_records",
                                   verbose = FALSE)

  expect_true(file.exists(file.path(out_dir, "fungorum_records.xlsx")))
  expect_gt(nrow(result), 0)
})


test_that("spfungorum_records renders a real HTML report when html_report = TRUE", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  .mock_fungorum_search(.fungorum_fixture())
  out_dir <- withr::local_tempdir()

  spfungorum_records(taxon = "Phellinotus", rank = "genus",
                         save = FALSE, html_report = TRUE, open_report = FALSE,
                         dir = out_dir, filename = "report_test", verbose = FALSE)

  expect_true(file.exists(file.path(out_dir, "report_test.html")))
})
