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


test_that("funga_distribution_gap flags a GBIF state not in FFB's official distribution", {
  .mock_funga_distribution_records("Trichoderma harzianum", ffb_states = "BA")
  .mock_occurrence_sources(gbif_states = data.frame(state = c("Bahia", "Pernambuco"), count = c(5L, 2L)))

  result <- funga_distribution_gap(taxon = "Trichoderma harzianum",
                                   sources = "gbif", save = FALSE, verbose = FALSE)

  expect_setequal(result$State, c("Bahia", "Pernambuco"))
  pe_row <- result[result$State == "Pernambuco", ]
  expect_true(pe_row$New_state_record_candidate)
  expect_false(pe_row$In_FFB_distribution)
  expect_equal(pe_row$GBIF_records, 2L)

  ba_row <- result[result$State == "Bahia", ]
  expect_false(ba_row$New_state_record_candidate)
  expect_true(ba_row$In_FFB_distribution)
})


test_that("funga_distribution_gap restricts to the requested 'state' argument", {
  .mock_funga_distribution_records("Trichoderma harzianum", ffb_states = "BA")
  .mock_occurrence_sources(gbif_states = data.frame(state = c("Bahia", "Pernambuco", "Ceara"),
                                                    count = c(1L, 1L, 1L)))

  result <- funga_distribution_gap(taxon = "Trichoderma harzianum", state = "Pernambuco",
                                   sources = "gbif", save = FALSE, verbose = FALSE)

  expect_equal(result$State, "Pernambuco")
})


test_that("funga_distribution_gap degrades gracefully when speciesLink returns no evidence", {
  .mock_funga_distribution_records("Trichoderma harzianum", ffb_states = "BA")
  .mock_occurrence_sources(gbif_states = data.frame(state = "Bahia", count = 1L),
                           splink_states = NULL)

  result <- funga_distribution_gap(taxon = "Trichoderma harzianum",
                                   sources = c("gbif", "speciesLink"), save = FALSE, verbose = FALSE)

  expect_true("speciesLink_evidence" %in% names(result))
  expect_false(any(result$speciesLink_evidence))
})


test_that("funga_distribution_gap treats an unmatched FFB taxon as having no official states", {
  .mock_funga_distribution_records("Some Other Taxon", ffb_states = character(0))
  .mock_occurrence_sources(gbif_states = data.frame(state = "Bahia", count = 1L))

  result <- funga_distribution_gap(taxon = "Trichoderma harzianum",
                                   sources = "gbif", save = FALSE, verbose = FALSE)

  expect_false(result$In_FFB_distribution[result$State == "Bahia"])
  expect_true(result$New_state_record_candidate[result$State == "Bahia"])
})


test_that("funga_distribution_gap returns an empty data.frame when no source has evidence", {
  .mock_funga_distribution_records("Trichoderma harzianum", ffb_states = character(0))
  .mock_occurrence_sources()

  result <- funga_distribution_gap(taxon = "Trichoderma harzianum",
                                   sources = c("gbif", "speciesLink"), save = FALSE, verbose = FALSE)

  expect_equal(nrow(result), 0)
})


test_that("funga_distribution_gap requires a single character taxon", {
  expect_error(funga_distribution_gap(taxon = NULL), "single character string")
  expect_error(funga_distribution_gap(taxon = c("A", "B")), "single character string")
})
