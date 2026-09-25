.funga_records_fixture <- function() {
  taxon_df <- data.frame(
    id = as.character(1:6),
    family = c("Hypocreaceae", "Hypocreaceae", "Hypocreaceae", "Marasmiaceae", "Marasmiaceae", "Hypocreaceae"),
    genus = c("Trichoderma", "Trichoderma", "Xylaria", "Marasmius", "Marasmius", "Trichoderma"),
    taxonName = c("Trichoderma harzianum", "Trichoderma viride", "Xylaria hypoxylon", "Marasmius alba",
                  "Marasmius beta", "Trichoderma synonym"),
    taxonRank = rep("ESPECIE", 6),
    taxonomicStatus = c(rep("NOME_ACEITO", 5), "SINONIMO"),
    stringsAsFactors = FALSE
  )
  # id=1 Trichoderma harzianum: Bahia + Minas Gerais, endemic
  # id=2 Trichoderma viride: Bahia, endemic
  # id=3 Xylaria hypoxylon: Sao Paulo, not endemic
  # id=4 Marasmius alba: Amazonas, not endemic
  # id=5 Marasmius beta: Para, not endemic
  # id=6 Trichoderma synonym: no distribution row (synonym, excluded from most filters anyway)
  distribution_df <- data.frame(
    id = c("1", "1", "2", "3", "4", "5"),
    locationID = c("BR-BA", "BR-MG", "BR-BA", "BR-SP", "BR-AM", "BR-PA"),
    phytogeographicDomain = c("Caatinga", "Mata Atlantica", "Caatinga", "Cerrado",
                              "Amazonia", "Amazonia"),
    endemism = c("true", "true", "true", "false", "false", "false"),
    stringsAsFactors = FALSE
  )
  speciesprofile_df <- data.frame(
    id = c("1", "2", "3"),
    lifeForm = c("Arbusto", "Arvore", "Erva"),
    habitat = c("Terricola", "Terricola", "Terricola"),
    vegetationType = c("Caatinga (stricto sensu)", "Caatinga (stricto sensu)",
                       "Cerrado (lato sensu)"),
    stringsAsFactors = FALSE
  )
  list(taxon_df = taxon_df, distribution_df = distribution_df, speciesprofile_df = speciesprofile_df)
}

.mock_funga_records <- function(fixture, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .funga_prepare_records = function(version, verbose, rm_funga_database) fixture,
    .package = "fungaR",
    .env = env
  )
}


test_that("funga_records with no filters returns the full taxon table", {
  fixture <- .funga_records_fixture()
  .mock_funga_records(fixture)

  result <- funga_records(verbose = FALSE)
  expect_equal(nrow(result), 6)
})


test_that("funga_records filters by taxon (family, genus, species)", {
  fixture <- .funga_records_fixture()
  .mock_funga_records(fixture)

  fam <- funga_records(taxon = "Marasmiaceae", verbose = FALSE)
  expect_setequal(fam$taxonName, c("Marasmius alba", "Marasmius beta"))

  gen <- funga_records(taxon = "Xylaria", verbose = FALSE)
  expect_equal(gen$taxonName, "Xylaria hypoxylon")

  sp <- funga_records(taxon = "Trichoderma harzianum", verbose = FALSE)
  expect_equal(sp$taxonName, "Trichoderma harzianum")
})


test_that("funga_records filters by taxonRank and taxonomicStatus", {
  fixture <- .funga_records_fixture()
  .mock_funga_records(fixture)

  accepted <- funga_records(taxonomicStatus = "NOME_ACEITO", verbose = FALSE)
  expect_false("Trichoderma synonym" %in% accepted$taxonName)
  expect_equal(nrow(accepted), 5)

  synonyms <- funga_records(taxonomicStatus = "sinonimo", verbose = FALSE)
  expect_equal(synonyms$taxonName, "Trichoderma synonym")

  species_rank <- funga_records(taxonRank = "ESPECIE", verbose = FALSE)
  expect_equal(nrow(species_rank), 6)
})


test_that("funga_records filters by state via the distribution table", {
  fixture <- .funga_records_fixture()
  .mock_funga_records(fixture)

  bahia <- funga_records(state = "Bahia", verbose = FALSE)
  expect_setequal(bahia$taxonName, c("Trichoderma harzianum", "Trichoderma viride"))

  # Acronym and diacritics-insensitive input should behave the same way
  bahia_abbrev <- funga_records(state = "BA", verbose = FALSE)
  expect_setequal(bahia_abbrev$taxonName, c("Trichoderma harzianum", "Trichoderma viride"))

  sp_state <- funga_records(state = "Sao Paulo", verbose = FALSE)
  expect_equal(sp_state$taxonName, "Xylaria hypoxylon")
})


test_that("funga_records filters by phytogeographicDomain", {
  fixture <- .funga_records_fixture()
  .mock_funga_records(fixture)

  caatinga <- funga_records(phytogeographicDomain = "Caatinga", verbose = FALSE)
  expect_setequal(caatinga$taxonName, c("Trichoderma harzianum", "Trichoderma viride"))
})


test_that("funga_records filters by endemism", {
  fixture <- .funga_records_fixture()
  .mock_funga_records(fixture)

  endemics <- funga_records(endemism = TRUE, verbose = FALSE)
  expect_setequal(endemics$taxonName, c("Trichoderma harzianum", "Trichoderma viride"))

  non_endemics <- funga_records(endemism = FALSE, verbose = FALSE)
  expect_setequal(non_endemics$taxonName, c("Xylaria hypoxylon", "Marasmius alba", "Marasmius beta"))
})


test_that("funga_records filters by lifeForm, habitat, and vegetationType", {
  fixture <- .funga_records_fixture()
  .mock_funga_records(fixture)

  shrubs <- funga_records(lifeForm = "Arbusto", verbose = FALSE)
  expect_equal(shrubs$taxonName, "Trichoderma harzianum")

  cerrado_veg <- funga_records(vegetationType = "Cerrado (lato sensu)", verbose = FALSE)
  expect_equal(cerrado_veg$taxonName, "Xylaria hypoxylon")
})


test_that("funga_records appends concatenated distribution/speciesprofile columns per taxon", {
  fixture <- .funga_records_fixture()
  .mock_funga_records(fixture)

  result <- funga_records(verbose = FALSE)

  expect_true(all(c("state", "phytogeographicDomain", "endemism",
                    "lifeForm", "habitat", "vegetationType") %in% names(result)))

  # id=1 (Trichoderma harzianum) has two distribution rows (Bahia, Minas
  # Gerais) which should be concatenated with " | ", not multiply the row.
  row1 <- result[result$taxonName == "Trichoderma harzianum", ]
  expect_equal(nrow(row1), 1)
  expect_setequal(strsplit(row1$state, " \\| ")[[1]], c("Bahia", "Minas Gerais"))
  expect_setequal(strsplit(row1$phytogeographicDomain, " \\| ")[[1]],
                  c("Caatinga", "Mata Atlantica"))

  # id=6 (Trichoderma synonym) has no distribution/speciesprofile rows at all
  row6 <- result[result$taxonName == "Trichoderma synonym", ]
  expect_true(is.na(row6$state))
  expect_true(is.na(row6$lifeForm))

  # Row order must be unchanged by the left-join aggregation
  expect_equal(result$taxonName, fixture$taxon_df$taxonName)
})


test_that("funga_records combines multiple filters with AND logic", {
  fixture <- .funga_records_fixture()
  .mock_funga_records(fixture)

  result <- funga_records(phytogeographicDomain = "Caatinga",
                          lifeForm = "Arbusto",
                          verbose = FALSE)
  expect_equal(result$taxonName, "Trichoderma harzianum")

  none <- funga_records(phytogeographicDomain = "Amazonia",
                        lifeForm = "Arbusto",
                        verbose = FALSE)
  expect_equal(nrow(none), 0)
})


test_that("funga_records saves a CSV file when save = TRUE", {
  fixture <- .funga_records_fixture()
  .mock_funga_records(fixture)

  tmp_dir <- tempfile("funga_records_save_")
  result <- funga_records(taxon = "Hypocreaceae",
                          save = TRUE,
                          dir = tmp_dir,
                          filename = "fabaceae_records",
                          verbose = FALSE)

  out <- file.path(tmp_dir, "fabaceae_records.csv")
  expect_true(file.exists(out))
  expect_gt(nrow(result), 0)

  unlink(tmp_dir, recursive = TRUE)
})


test_that("funga_records prints a summary message when verbose = TRUE", {
  fixture <- .funga_records_fixture()
  .mock_funga_records(fixture)

  expect_message(funga_records(taxon = "Hypocreaceae", verbose = TRUE),
                 "Returned \\d+ taxon record")
})


test_that(".funga_prepare_records extracts taxon/distribution/speciesprofile tables", {
  dwca <- list(
    dwca_ffb_v1 = list(data = list(
      taxon.txt = data.frame(id = "1", taxonName = "Trichoderma harzianum", stringsAsFactors = FALSE),
      distribution.txt = data.frame(id = "1", locationID = "BR-BA", stringsAsFactors = FALSE),
      speciesprofile.txt = data.frame(id = "1", lifeForm = "Arbusto", stringsAsFactors = FALSE)
    ))
  )

  testthat::local_mocked_bindings(
    funga_download = function(version, dir, verbose) invisible(NULL),
    funga_parse = function(path, version, verbose) dwca,
    .package = "fungaR"
  )

  result <- .funga_prepare_records(version = "latest", verbose = FALSE, rm_funga_database = FALSE)
  expect_equal(result$taxon_df$taxonName, "Trichoderma harzianum")
  expect_equal(result$distribution_df$locationID, "BR-BA")
  expect_equal(result$speciesprofile_df$lifeForm, "Arbusto")
})
