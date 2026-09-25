.funga_search_fixture <- function() {
  taxon_df <- data.frame(
    id = as.character(1:5),
    family = c("Hypocreaceae", "Hypocreaceae", "Hypocreaceae", "Boletaceae", "Hypocreaceae"),
    genus = c("Trichoderma", "Trichoderma", "Xylaria", "Boletus", "Xylaria"),
    specificEpithet = c("harzianum", "viride", "hypoxylon", "edulis", "cornuta"),
    infraspecificEpithet = c(NA, NA, NA, NA, NA),
    taxonName = c("Trichoderma harzianum", "Trichoderma viride", "Xylaria hypoxylon",
                  "Boletus edulis", "Xylaria cornuta"),
    taxonRank = rep("ESPECIE", 5),
    taxonomicStatus = c("NOME_ACEITO", "NOME_ACEITO", "NOME_ACEITO",
                        "NOME_ACEITO", "SINONIMO"),
    acceptedNameUsageID = c(NA, NA, NA, NA, "3"),
    scientificNameAuthorship = c("Mart.", "Willd.", "L.", "L.", "Poir."),
    order = rep("Hypocreales", 5),
    stringsAsFactors = FALSE
  )

  genus_index <- .funga_build_genus_index(taxon_df)
  id_lookup <- tapply(seq_len(nrow(taxon_df)), taxon_df$id, unique, simplify = FALSE)

  list(taxon_df = taxon_df, genus_index = genus_index, id_lookup = id_lookup)
}

.mock_funga_search <- function(fixture, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .funga_prepare_taxon = function(version, verbose, rm_funga_database) fixture,
    .package = "fungaR",
    .env = env
  )
}


test_that("funga_search finds exact matches", {
  .mock_funga_search(.funga_search_fixture())

  result <- funga_search("Trichoderma harzianum", progress_bar = FALSE, verbose = FALSE)
  expect_equal(result$Accepted.taxon.Name, "Trichoderma harzianum")
  expect_equal(result$taxonomicStatus, "NOME_ACEITO")
  expect_equal(result$family, "Hypocreaceae")
})


test_that("funga_search resolves a synonym to its accepted name", {
  .mock_funga_search(.funga_search_fixture())

  result <- funga_search("Xylaria cornuta", progress_bar = FALSE, verbose = FALSE)
  expect_equal(result$taxonomicStatus, "SINONIMO")
  expect_equal(result$Accepted.taxon.Name, "Xylaria hypoxylon")
})


test_that("funga_search performs fuzzy matching within max_distance", {
  .mock_funga_search(.funga_search_fixture())

  expect_warning(
    result <- funga_search("Trichoderma harzianun", max_distance = 0.2, genus_fuzzy = FALSE,
                           progress_bar = FALSE, verbose = FALSE, show_correct = TRUE),
    NA
  )
  expect_equal(result$Accepted.taxon.Name, "Trichoderma harzianum")
  expect_false(result$Correct.Spelling)
})


test_that("funga_search returns an NA row for an unmatched name mixed with a real match", {
  .mock_funga_search(.funga_search_fixture())

  expect_warning(
    result <- funga_search(c("Trichoderma harzianum", "Xyzabcus completelyfake"), max_distance = 0.1,
                           progress_bar = FALSE, verbose = FALSE),
    "No match found for 'Xyzabcus completelyfake'"
  )
  expect_equal(result$Accepted.taxon.Name[1], "Trichoderma harzianum")
  expect_true(is.na(result$FFB.taxon.ID[2]))
})


test_that("funga_search returns NULL and warns when a single name has no match at all", {
  .mock_funga_search(.funga_search_fixture())

  # Two warnings are raised in sequence: one for the specific unmatched name,
  # and one final summary warning since ALL inputs failed to match.
  expect_warning(
    expect_warning(
      result <- funga_search("Xyzabcus completelyfake", max_distance = 0.1,
                             progress_bar = FALSE, verbose = FALSE),
      "No match found for 'Xyzabcus completelyfake'"
    ),
    "No match found for any input name"
  )
  expect_null(result)
})


test_that("funga_search searches multiple names at once", {
  .mock_funga_search(.funga_search_fixture())

  splist <- c("Trichoderma harzianum", "Xylaria hypoxylon")
  result <- funga_search(splist, progress_bar = FALSE, verbose = FALSE)
  expect_equal(nrow(result), 2)
  expect_equal(result$Search, splist)
})


test_that("funga_search warns when an input name lacks an epithet", {
  .mock_funga_search(.funga_search_fixture())

  expect_warning(
    result <- funga_search(c("Trichoderma harzianum", "Trichoderma"), progress_bar = FALSE, verbose = FALSE),
    "does not include an epithet"
  )
  expect_true(is.na(result$FFB.taxon.ID[2]))
})
