.mock_fungorum_search <- function(fixture, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .indexfungorum_name_search = function(taxon, rank, max_number) fixture,
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
    Year = c("2022", "2022", "2016", "2016"),
    Rank = c("sp.", "sp.", "sp.", "sp."),
    Name_status = c("Legitimate", "Legitimate", "Legitimate", "Legitimate"),
    Current_name = c("Phellinotus badius", "Phellinotus magnoporatus",
                     "Fomitiporella neoarida", "Phellinotus piptadeniae"),
    Location = c(NA_character_, "Peru", "Pernambuco", NA_character_),
    Host = c(NA_character_, "on living tree of Ocotea aurantiodora",
            "on Caesalpinia", NA_character_),
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
                                   Year = character(0), Rank = character(0),
                                   Name_status = character(0), Current_name = character(0),
                                   Location = character(0), Host = character(0),
                                   Fungorum_Number = character(0), stringsAsFactors = FALSE))

  result <- spfungorum_records(taxon = "Nonexistentus", rank = "genus",
                                   save = FALSE, html_report = FALSE, verbose = FALSE)

  expect_equal(nrow(result), 0)
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
