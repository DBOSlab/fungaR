.funga_match_fixture <- function() {
  taxon_df <- data.frame(
    id = as.character(1:4),
    family = rep("Xylariaceae", 4),
    genus = rep("Xylaria", 4),
    specificEpithet = c("polymorpha", "hypoxylon", "cornuta", "polymorpha"),
    infraspecificEpithet = c(NA, NA, NA, "minor"),
    taxonName = c("Xylaria polymorpha", "Xylaria hypoxylon", "Xylaria cornuta",
                  "Xylaria polymorpha var. minor"),
    taxonRank = c("ESPECIE", "ESPECIE", "ESPECIE", "VARIEDADE"),
    taxonomicStatus = c("NOME_ACEITO", "NOME_ACEITO", "SINONIMO", "NOME_ACEITO"),
    acceptedNameUsageID = c(NA, NA, "2", NA),
    scientificNameAuthorship = c("(Pers.) Grev.", "(L.) Grev.", "(Kunze) Berk.", "J.D. Rogers"),
    order = rep("Xylariales", 4),
    stringsAsFactors = FALSE
  )

  genus_index <- .funga_build_genus_index(taxon_df)
  id_lookup <- tapply(seq_len(nrow(taxon_df)), taxon_df$id, unique, simplify = FALSE)

  list(taxon_df = taxon_df, genus_index = genus_index, id_lookup = id_lookup)
}

.mock_funga_match <- function(fixture, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .funga_prepare_taxon = function(version, verbose, rm_funga_database) fixture,
    .package = "fungaR",
    .env = env
  )
}


test_that("funga_match aligns two name lists that resolve to the same accepted taxon", {
  .mock_funga_match(.funga_match_fixture())

  splist1 <- c("Xylaria polymorpha", "Xylaria hypoxylon")
  splist2 <- c("Xylaria cornuta", "Xylaria polymorpha")  # order intentionally swapped

  result <- funga_match(splist1, splist2, progress_bar = FALSE, verbose = FALSE)

  expect_equal(result$Species.List.1, splist1)
  # Xylaria hypoxylon (splist1) should align with Xylaria cornuta (splist2), a synonym
  expect_equal(result$Species.List.2[result$Species.List.1 == "Xylaria hypoxylon"], "Xylaria cornuta")
  expect_equal(result$Species.List.2[result$Species.List.1 == "Xylaria polymorpha"], "Xylaria polymorpha")
})


test_that("funga_match appends splist2-only names when include_all = TRUE", {
  .mock_funga_match(.funga_match_fixture())

  splist1 <- c("Xylaria polymorpha")
  splist2 <- c("Xylaria polymorpha", "Xylaria polymorpha var. minor")

  result <- funga_match(splist1, splist2, include_all = TRUE,
                        progress_bar = FALSE, verbose = FALSE)

  expect_true("Xylaria polymorpha var. minor" %in% result$Species.List.2)
  extra_row <- result[result$Species.List.2 == "Xylaria polymorpha var. minor", ]
  expect_true(is.na(extra_row$Species.List.1))
})


test_that("funga_match omits splist2-only names when include_all = FALSE", {
  .mock_funga_match(.funga_match_fixture())

  splist1 <- c("Xylaria polymorpha")
  splist2 <- c("Xylaria polymorpha", "Xylaria polymorpha var. minor")

  result <- funga_match(splist1, splist2, include_all = FALSE,
                        progress_bar = FALSE, verbose = FALSE)

  expect_equal(nrow(result), 1)
})


test_that("funga_match flags duplicated accepted names when identify_dups = TRUE", {
  .mock_funga_match(.funga_match_fixture())

  splist1 <- c("Xylaria hypoxylon", "Xylaria cornuta")  # both resolve to Xylaria hypoxylon
  splist2 <- c("Xylaria polymorpha", "Xylaria polymorpha")

  result <- funga_match(splist1, splist2, identify_dups = TRUE,
                        progress_bar = FALSE, verbose = FALSE)

  expect_true("Duplicated.Output.Position" %in% names(result))
  expect_false(all(is.na(result$Duplicated.Output.Position)))
})


test_that("funga_match provides Match.Position.2to1 to re-order splist2", {
  .mock_funga_match(.funga_match_fixture())

  splist1 <- c("Xylaria polymorpha", "Xylaria hypoxylon")
  splist2 <- c("Xylaria hypoxylon", "Xylaria polymorpha")

  result <- funga_match(splist1, splist2, include_all = FALSE,
                        progress_bar = FALSE, verbose = FALSE)

  expect_equal(splist2[result$Match.Position.2to1], splist1)
})


test_that("funga_match errors when splist1 has no matches at all", {
  .mock_funga_match(.funga_match_fixture())

  expect_error(
    suppressWarnings(
      funga_match("Totallyfake completelynotreal", "Xylaria polymorpha",
                 max_distance = 0.01, progress_bar = FALSE, verbose = FALSE)
    ),
    "No match found for splist1"
  )
})
