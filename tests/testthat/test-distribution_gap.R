.mock_funga_distribution_records <- function(taxon_name, ffb_states, env = parent.frame()) {
  taxon_df <- data.frame(id = "1", taxonName = taxon_name, stringsAsFactors = FALSE)
  distribution_df <- if (length(ffb_states) == 0) {
    data.frame(id = character(0), locationID = character(0), stringsAsFactors = FALSE)
  } else {
    data.frame(id = rep("1", length(ffb_states)), locationID = paste0("BR-", ffb_states),
              stringsAsFactors = FALSE)
  }
  testthat::local_mocked_bindings(
    .funga_prepare_records = function(version, verbose, rm_funga_database) {
      list(taxon_df = taxon_df, distribution_df = distribution_df, speciesprofile_df = data.frame())
    },
    .package = "fungaR",
    .env = env
  )
}

.mock_occurrence_sources <- function(gbif_states = NULL, splink_states = NULL, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .gbif_brazil_occurrence = function(name) {
      if (is.null(gbif_states)) {
        return(list(n_records = NA_integer_, states = NA_character_,
                    state_counts = data.frame(state = character(0), count = integer(0))))
      }
      list(n_records = sum(gbif_states$count), states = paste(gbif_states$state, collapse = "; "),
          state_counts = gbif_states)
    },
    .gbif_brazil_state_records = function(name, state, limit = 50) {
      data.frame(key = integer(0), scientificName = character(0), stateProvince = character(0),
                municipality = character(0), locality = character(0), recordedBy = character(0),
                recordNumber = character(0), eventDate = character(0), institutionCode = character(0),
                collectionCode = character(0), catalogNumber = character(0),
                decimalLatitude = numeric(0), decimalLongitude = numeric(0),
                GBIF_URL = character(0), stringsAsFactors = FALSE)
    },
    .splink_brazil_occurrence = function(name) {
      if (is.null(splink_states)) {
        return(list(n_records = NA_integer_, states = NA_character_))
      }
      list(n_records = length(splink_states), states = paste(splink_states, collapse = "; "))
    },
    .package = "fungaR",
    .env = env
  )
}


test_that("distribution_gap renders a real HTML report when html_report = TRUE", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  .mock_funga_distribution_records("Trichoderma harzianum", ffb_states = "BA")
  .mock_occurrence_sources(gbif_states = data.frame(state = c("Bahia", "Pernambuco"), count = c(5L, 2L)))

  out_dir <- withr::local_tempdir()

  result <- distribution_gap(taxon = "Trichoderma harzianum",
                                   sources = "gbif", save = FALSE, html_report = TRUE,
                                   open_report = FALSE, dir = out_dir,
                                   filename = "report_test", verbose = FALSE)

  expect_true(file.exists(file.path(out_dir, "report_test.html")))
})


test_that("distribution_gap flags a GBIF state not in FFB's official distribution", {
  .mock_funga_distribution_records("Trichoderma harzianum", ffb_states = "BA")
  .mock_occurrence_sources(gbif_states = data.frame(state = c("Bahia", "Pernambuco"), count = c(5L, 2L)))

  result <- distribution_gap(taxon = "Trichoderma harzianum",
                                   sources = "gbif", save = FALSE, html_report = FALSE, verbose = FALSE)

  expect_setequal(result$State, c("Bahia", "Pernambuco"))
  pe_row <- result[result$State == "Pernambuco", ]
  expect_true(pe_row$New_state_record_candidate)
  expect_false(pe_row$In_FFB_distribution)
  expect_equal(pe_row$GBIF_records, 2L)

  ba_row <- result[result$State == "Bahia", ]
  expect_false(ba_row$New_state_record_candidate)
  expect_true(ba_row$In_FFB_distribution)
})


test_that("distribution_gap restricts to the requested 'state' argument", {
  .mock_funga_distribution_records("Trichoderma harzianum", ffb_states = "BA")
  .mock_occurrence_sources(gbif_states = data.frame(state = c("Bahia", "Pernambuco", "Ceara"),
                                                    count = c(1L, 1L, 1L)))

  result <- distribution_gap(taxon = "Trichoderma harzianum", state = "Pernambuco",
                                   sources = "gbif", save = FALSE, html_report = FALSE, verbose = FALSE)

  expect_equal(result$State, "Pernambuco")
})


test_that("distribution_gap degrades gracefully when speciesLink returns no evidence", {
  .mock_funga_distribution_records("Trichoderma harzianum", ffb_states = "BA")
  .mock_occurrence_sources(gbif_states = data.frame(state = "Bahia", count = 1L),
                           splink_states = NULL)

  result <- distribution_gap(taxon = "Trichoderma harzianum",
                                   sources = c("gbif", "speciesLink"), save = FALSE, html_report = FALSE, verbose = FALSE)

  expect_true("speciesLink_evidence" %in% names(result))
  expect_false(any(result$speciesLink_evidence))
})


test_that("distribution_gap treats an unmatched FFB taxon as having no official states", {
  .mock_funga_distribution_records("Some Other Taxon", ffb_states = character(0))
  .mock_occurrence_sources(gbif_states = data.frame(state = "Bahia", count = 1L))

  result <- distribution_gap(taxon = "Trichoderma harzianum",
                                   sources = "gbif", save = FALSE, html_report = FALSE, verbose = FALSE)

  expect_false(result$In_FFB_distribution[result$State == "Bahia"])
  expect_true(result$New_state_record_candidate[result$State == "Bahia"])
})


test_that("distribution_gap returns an empty data.frame when no source has evidence", {
  .mock_funga_distribution_records("Trichoderma harzianum", ffb_states = character(0))
  .mock_occurrence_sources()

  result <- distribution_gap(taxon = "Trichoderma harzianum",
                                   sources = c("gbif", "speciesLink"), save = FALSE, html_report = FALSE, verbose = FALSE)

  expect_equal(nrow(result), 0)
})


test_that("distribution_gap requires a single character taxon", {
  expect_error(distribution_gap(taxon = NULL), "single character string")
  expect_error(distribution_gap(taxon = c("A", "B")), "single character string")
})


test_that("distribution_gap writes a real .xlsx file when save = TRUE", {
  .mock_funga_distribution_records("Trichoderma harzianum", ffb_states = "BA")
  .mock_occurrence_sources(gbif_states = data.frame(state = c("Bahia", "Pernambuco"), count = c(5L, 2L)))
  out_dir <- withr::local_tempdir()

  result <- distribution_gap(taxon = "Trichoderma harzianum",
                                   sources = "gbif", save = TRUE, html_report = FALSE,
                                   dir = out_dir, filename = "dist_gap", verbose = FALSE)

  expect_true(file.exists(file.path(out_dir, "dist_gap.xlsx")))
  expect_gt(nrow(result), 0)
})


test_that("distribution_gap includes REFLORA evidence when refloraR is installed", {
  skip_if_not_installed("refloraR")

  .mock_funga_distribution_records("Trichoderma harzianum", ffb_states = "BA")
  .mock_occurrence_sources()

  fake_reflora <- data.frame(
    stateProvince = c("Pernambuco", "Pernambuco", "Bahia"),
    municipality = c("Recife", "Caruaru", "Salvador"),
    locality = c("Loc 1", "Loc 2", "Loc 3"),
    recordedBy = c("Silva", "Souza", "Costa"),
    recordNumber = c("1", "2", "3"),
    herbarium = c("RB", "RB", "RB"),
    bibliographicCitation = c("https://reflora/1", "https://reflora/2", "https://reflora/3"),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    reflora_records = function(taxon, verbose, save) fake_reflora,
    .package = "refloraR"
  )

  result <- distribution_gap(taxon = "Trichoderma harzianum",
                                   sources = "reflora", save = FALSE, html_report = FALSE,
                                   verbose = FALSE)

  expect_true("REFLORA_records" %in% names(result))
  pe_row <- result[result$State == "Pernambuco", ]
  expect_equal(pe_row$REFLORA_records, 2L)
  expect_true(pe_row$New_state_record_candidate)
})


test_that("distribution_gap skips REFLORA gracefully when refloraR is not installed", {
  testthat::local_mocked_bindings(
    requireNamespace = function(pkg, ...) if (identical(pkg, "refloraR")) FALSE else TRUE,
    .package = "base"
  )

  .mock_funga_distribution_records("Trichoderma harzianum", ffb_states = "BA")
  .mock_occurrence_sources(gbif_states = data.frame(state = "Bahia", count = 1L))

  result <- distribution_gap(taxon = "Trichoderma harzianum",
                                   sources = c("gbif", "reflora"), save = FALSE,
                                   html_report = FALSE, verbose = FALSE)

  expect_false("REFLORA_records" %in% names(result) && any(!is.na(result$REFLORA_records)))
})


test_that("distribution_gap populates records_detail for new-state GBIF candidates in the HTML report", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  .mock_funga_distribution_records("Trichoderma harzianum", ffb_states = "BA")
  .mock_occurrence_sources(gbif_states = data.frame(state = c("Bahia", "Pernambuco"), count = c(5L, 2L)))
  testthat::local_mocked_bindings(
    .gbif_brazil_state_records = function(name, state, limit = 50) {
      data.frame(key = 999, scientificName = name, stateProvince = state,
                municipality = "Recife", locality = "Loc", recordedBy = "Someone",
                recordNumber = "1", eventDate = "2020-01-01", institutionCode = "UFPE",
                collectionCode = NA, catalogNumber = "abc",
                decimalLatitude = -8, decimalLongitude = -36,
                GBIF_URL = "https://www.gbif.org/occurrence/999", stringsAsFactors = FALSE)
    },
    .package = "fungaR"
  )

  out_dir <- withr::local_tempdir()
  distribution_gap(taxon = "Trichoderma harzianum", sources = "gbif", save = FALSE,
                        html_report = TRUE, open_report = FALSE, dir = out_dir,
                        filename = "records_detail_test", verbose = FALSE)

  expect_true(file.exists(file.path(out_dir, "records_detail_test.html")))
  html_txt <- paste(readLines(file.path(out_dir, "records_detail_test.html"), warn = FALSE), collapse = "\n")
  expect_true(grepl("gbif.org/occurrence/999", html_txt, fixed = TRUE))
})
