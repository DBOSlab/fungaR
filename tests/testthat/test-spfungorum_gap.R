.mock_fungorum_gap_search <- function(fixture, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .indexfungorum_name_search = function(taxon, rank, max_number) fixture,
    .package = "fungaR",
    .env = env
  )
}

.mock_fungorum_gap_ffb <- function(taxon_names, env = parent.frame()) {
  testthat::local_mocked_bindings(
    funga_get_children_taxa = function(taxon_name, rank, child_rank, include_synonyms, version, verbose) {
      data.frame(taxonName = taxon_names, stringsAsFactors = FALSE)
    },
    .package = "fungaR",
    .env = env
  )
}

.fungorum_gap_fixture <- function() {
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
    Location = c(NA_character_, "Peru", "Pernambuco", "Bahia"),
    Host = c(NA_character_, "on living tree of Ocotea aurantiodora",
            "on Caesalpinia", NA_character_),
    Fungorum_Number = c("840993", "840994", "805901", "805902"),
    stringsAsFactors = FALSE
  )
}


test_that("spfungorum_gap flags Index Fungorum species with Brazil evidence missing from FFB", {
  .mock_fungorum_gap_search(.fungorum_gap_fixture())
  .mock_fungorum_gap_ffb(character(0))  # nothing yet in FFB

  result <- spfungorum_gap(taxon = "Phellinotus", save = FALSE, html_report = FALSE,
                               verbose = FALSE)

  expect_setequal(result$Taxon_name, c("Phellinotus neoaridus", "Phellinotus piptadeniae"))
  expect_true(all(result$Fungorum_Brazil_Evidence))
  expect_true(all(!result$In_FFB))
})


test_that("spfungorum_gap does not flag a name as missing when its raw Index Fungorum name matches FFB even if its Current name points to a different genus", {
  # Regression check mirroring the same Phellinotus/Fomitiporella disagreement
  # found for mycobank_gap(): FFB accepts "Phellinotus neoaridus" itself,
  # even though Index Fungorum's Current name redirects it to "Fomitiporella
  # neoarida". Checking only the effective/current name would wrongly flag it.
  .mock_fungorum_gap_search(.fungorum_gap_fixture())
  .mock_fungorum_gap_ffb(c("Phellinotus neoaridus"))

  result <- spfungorum_gap(taxon = "Phellinotus", save = FALSE, html_report = FALSE,
                               verbose = FALSE)

  expect_false("Phellinotus neoaridus" %in% result$Taxon_name)
  expect_true("Phellinotus piptadeniae" %in% result$Taxon_name)
})


test_that("spfungorum_gap excludes candidates without Brazil evidence by default", {
  .mock_fungorum_gap_search(.fungorum_gap_fixture())
  .mock_fungorum_gap_ffb(character(0))

  result <- spfungorum_gap(taxon = "Phellinotus", save = FALSE, html_report = FALSE,
                               verbose = FALSE)

  expect_false("Phellinotus badius" %in% result$Taxon_name)
  expect_false("Phellinotus magnoporatus" %in% result$Taxon_name)
})


test_that("spfungorum_gap returns unfiltered missing names when require_brazil_evidence = FALSE", {
  .mock_fungorum_gap_search(.fungorum_gap_fixture())
  .mock_fungorum_gap_ffb(character(0))

  result <- spfungorum_gap(taxon = "Phellinotus", require_brazil_evidence = FALSE,
                               save = FALSE, html_report = FALSE, verbose = FALSE)

  expect_equal(nrow(result), 4)
})


test_that("spfungorum_gap treats an unmatched FFB taxon as an empty known-species set", {
  .mock_fungorum_gap_search(.fungorum_gap_fixture())
  testthat::local_mocked_bindings(
    funga_get_children_taxa = function(...) stop("Taxon 'Phellinotus' with rank 'genus' not found in FFB database."),
    .package = "fungaR"
  )

  result <- spfungorum_gap(taxon = "Phellinotus", require_brazil_evidence = FALSE,
                               save = FALSE, html_report = FALSE, verbose = FALSE)

  expect_equal(nrow(result), 4)
})


test_that("spfungorum_gap returns an empty data.frame (not an error) when every candidate is already in FFB", {
  .mock_fungorum_gap_search(.fungorum_gap_fixture())
  .mock_fungorum_gap_ffb(c("Phellinotus badius", "Phellinotus magnoporatus",
                           "Phellinotus neoaridus", "Phellinotus piptadeniae"))

  result <- spfungorum_gap(taxon = "Phellinotus", require_brazil_evidence = FALSE,
                               save = FALSE, html_report = FALSE, verbose = FALSE)

  expect_equal(nrow(result), 0)
  expect_true("In_FFB" %in% names(result))
})


test_that("spfungorum_gap requires a single character taxon", {
  expect_error(spfungorum_gap(taxon = NULL), "single character string")
  expect_error(spfungorum_gap(taxon = c("A", "B")), "single character string")
})


test_that("spfungorum_gap writes a real .xlsx file when save = TRUE", {
  .mock_fungorum_gap_search(.fungorum_gap_fixture())
  .mock_fungorum_gap_ffb(character(0))
  out_dir <- withr::local_tempdir()

  result <- spfungorum_gap(taxon = "Phellinotus", save = TRUE, html_report = FALSE,
                               dir = out_dir, filename = "fungorum_gap", verbose = FALSE)

  expect_true(file.exists(file.path(out_dir, "fungorum_gap.xlsx")))
  expect_gt(nrow(result), 0)
})


test_that("spfungorum_gap renders a real HTML report when html_report = TRUE", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  .mock_fungorum_gap_search(.fungorum_gap_fixture())
  .mock_fungorum_gap_ffb(character(0))
  out_dir <- withr::local_tempdir()

  spfungorum_gap(taxon = "Phellinotus", save = FALSE, html_report = TRUE,
                     open_report = FALSE, dir = out_dir, filename = "report_test",
                     verbose = FALSE)

  expect_true(file.exists(file.path(out_dir, "report_test.html")))
})
