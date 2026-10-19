.funga_records_fixture <- function() {
  taxon_df <- data.frame(
    id = as.character(1:6),
    family = c("Hypocreaceae", "Hypocreaceae", "Hygrophoraceae", "Xylariaceae",
              "Xylariaceae", "Hypocreaceae"),
    genus = c("Trichoderma", "Trichoderma", "Hygrocybe", "Xylaria", "Xylaria", "Trichoderma"),
    taxonName = c("Trichoderma harzianum", "Trichoderma viride", "Hygrocybe conica",
                  "Xylaria polymorpha", "Xylaria cornuta", "Trichoderma synonym"),
    taxonRank = rep("ESPECIE", 6),
    taxonomicStatus = c(rep("NOME_ACEITO", 5), "SINONIMO"),
    stringsAsFactors = FALSE
  )
  # id=1 Trichoderma harzianum: Bahia + Minas Gerais, endemic
  # id=2 Trichoderma viride: Bahia, endemic
  # id=3 Hygrocybe conica: Sao Paulo, not endemic
  # id=4 Xylaria polymorpha: Amazonas, not endemic
  # id=5 Xylaria cornuta: Para, not endemic
  # id=6 Trichoderma synonym: no distribution row (synonym, excluded from most filters anyway)
  distribution_df <- data.frame(
    id = c("1", "1", "2", "3", "4", "5"),
    locationID = c("BR-BA", "BR-MG", "BR-BA", "BR-SP", "BR-AM", "BR-PA"),
    phytogeographicDomain = c("Caatinga", "Mata Atlantica", "Caatinga", "Cerrado",
                              "Amazonia", "Amazonia"),
    endemism = c("true", "true", "true", "false", "false", "false"),
    establishmentMeans = c("Nativa", "Nativa", "Nativa", "Ex\u00f3tica", "Nativa", "Ex\u00f3tica"),
    stringsAsFactors = FALSE
  )
  speciesprofile_df <- data.frame(
    id = c("1", "2", "3"),
    lifeForm = c("Saprobio", "Liquenizado", "Micorrizico"),
    habitat = c("Solo", "Solo", "Solo"),
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

  fam <- funga_records(taxon = "Xylariaceae", verbose = FALSE)
  expect_setequal(fam$taxonName, c("Xylaria polymorpha", "Xylaria cornuta"))

  gen <- funga_records(taxon = "Hygrocybe", verbose = FALSE)
  expect_equal(gen$taxonName, "Hygrocybe conica")

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
  expect_equal(sp_state$taxonName, "Hygrocybe conica")
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
  expect_setequal(non_endemics$taxonName,
                  c("Hygrocybe conica", "Xylaria polymorpha", "Xylaria cornuta"))
})


test_that("funga_records with endemism = TRUE and a state returns the endemics exclusive to it", {
  # Regression: endemism = TRUE with a state used to return every Brazilian
  # endemic that merely occurs in the state (Trichoderma harzianum occurs in BA and MG)
  .mock_funga_records(.funga_records_fixture())

  bahia_endemics <- funga_records(state = "Bahia", endemism = TRUE, verbose = FALSE)
  expect_equal(bahia_endemics$taxonName, "Trichoderma viride")
})


test_that("funga_records with endemism = 'Brazil' returns the Brazilian endemics occurring in the areas", {
  .mock_funga_records(.funga_records_fixture())

  in_bahia <- funga_records(state = "Bahia", endemism = "Brazil", verbose = FALSE)
  expect_setequal(in_bahia$taxonName, c("Trichoderma harzianum", "Trichoderma viride"))
  # Without an area, "Brazil" is the same as TRUE
  expect_setequal(funga_records(endemism = "Brazil", verbose = FALSE)$taxonName,
                  funga_records(endemism = TRUE, verbose = FALSE)$taxonName)
})


test_that("funga_records with endemism = FALSE and a state returns the taxa not exclusive to it", {
  .mock_funga_records(.funga_records_fixture())

  not_exclusive <- funga_records(state = "Bahia", endemism = FALSE, verbose = FALSE)
  expect_equal(not_exclusive$taxonName, "Trichoderma harzianum")
  # TRUE and FALSE partition the taxa occurring in the area
  expect_setequal(c(not_exclusive$taxonName,
                    funga_records(state = "Bahia", endemism = TRUE, verbose = FALSE)$taxonName),
                  funga_records(state = "Bahia", verbose = FALSE)$taxonName)
})


test_that("funga_records treats several states as one area for exclusive endemism", {
  .mock_funga_records(.funga_records_fixture())

  ba_mg <- funga_records(state = c("BA", "Minas Gerais"), endemism = TRUE, verbose = FALSE)
  expect_setequal(ba_mg$taxonName, c("Trichoderma harzianum", "Trichoderma viride"))
})


test_that("funga_records applies exclusive endemism to domains and vegetation types", {
  .mock_funga_records(.funga_records_fixture())

  # Trichoderma harzianum is also in the Mata Atlantica, so it is not a Caatinga endemic
  caatinga <- funga_records(phytogeographicDomain = "Caatinga", endemism = TRUE, verbose = FALSE)
  expect_equal(caatinga$taxonName, "Trichoderma viride")

  veg <- funga_records(vegetationType = "Caatinga (stricto sensu)", endemism = TRUE, verbose = FALSE)
  expect_setequal(veg$taxonName, c("Trichoderma harzianum", "Trichoderma viride"))
})


test_that("funga_records requires exclusivity to every area filter combined", {
  .mock_funga_records(.funga_records_fixture())

  ba_caatinga <- funga_records(state = "Bahia", phytogeographicDomain = "Caatinga",
                               endemism = TRUE, verbose = FALSE)
  expect_equal(ba_caatinga$taxonName, "Trichoderma viride")

  # Trichoderma harzianum is restricted to BA + MG, but not to the Caatinga
  ba_mg_caatinga <- funga_records(state = c("BA", "MG"), phytogeographicDomain = "Caatinga",
                                  endemism = TRUE, verbose = FALSE)
  expect_equal(ba_mg_caatinga$taxonName, "Trichoderma viride")
})


test_that("funga_records excludes taxa with no record for an area from its exclusive endemics", {
  fixture <- .funga_records_fixture()
  # A Bahia-only endemic whose state is not recorded cannot be shown to be exclusive
  fixture$distribution_df$locationID[fixture$distribution_df$id == "2"] <- NA
  .mock_funga_records(fixture)

  expect_equal(nrow(funga_records(state = "Bahia", endemism = TRUE, verbose = FALSE)), 0)
})


test_that("funga_records matches area and trait values regardless of accents and case", {
  .mock_funga_records(.funga_records_fixture())

  expect_equal(funga_records(phytogeographicDomain = "Mata Atl\u00e2ntica", verbose = FALSE)$taxonName,
               "Trichoderma harzianum")
  expect_equal(funga_records(lifeForm = "LIQUENIZADO", verbose = FALSE)$taxonName,
               "Trichoderma viride")
})


test_that("funga_records rejects an invalid endemism value", {
  .mock_funga_records(.funga_records_fixture())
  expect_error(funga_records(endemism = "yes", verbose = FALSE),
               "must be TRUE, FALSE, \"Brazil\", or NULL")
})


test_that("funga_records filters by establishmentMeans (origin), in English or Portuguese", {
  .mock_funga_records(.funga_records_fixture())

  introduced <- funga_records(establishmentMeans = "introduced", verbose = FALSE)
  expect_setequal(introduced$taxonName, c("Hygrocybe conica", "Xylaria cornuta"))
  expect_setequal(funga_records(establishmentMeans = "Ex\u00f3tica", verbose = FALSE)$taxonName,
                  introduced$taxonName)

  native <- funga_records(establishmentMeans = "NATIVA", verbose = FALSE)
  expect_setequal(native$taxonName,
                  c("Trichoderma harzianum", "Trichoderma viride", "Xylaria polymorpha"))
  expect_equal(nrow(funga_records(establishmentMeans = c("native", "introduced"), verbose = FALSE)), 5)
})


test_that("funga_records combines establishmentMeans with the other filters", {
  .mock_funga_records(.funga_records_fixture())
  expect_equal(nrow(funga_records(state = "Bahia", establishmentMeans = "introduced", verbose = FALSE)), 0)
  expect_equal(funga_records(state = "Bahia", establishmentMeans = "native", endemism = TRUE,
                             verbose = FALSE)$taxonName, "Trichoderma viride")
})


test_that("funga_records rejects an unrecognised establishmentMeans", {
  .mock_funga_records(.funga_records_fixture())
  expect_error(funga_records(establishmentMeans = "cultivated", verbose = FALSE), "Unrecognised 'establishmentMeans'")
})


test_that("funga_records filters by lifeForm, habitat, and vegetationType", {
  fixture <- .funga_records_fixture()
  .mock_funga_records(fixture)

  saprobes <- funga_records(lifeForm = "Saprobio", verbose = FALSE)
  expect_equal(saprobes$taxonName, "Trichoderma harzianum")

  cerrado_veg <- funga_records(vegetationType = "Cerrado (lato sensu)", verbose = FALSE)
  expect_equal(cerrado_veg$taxonName, "Hygrocybe conica")
})


test_that("funga_records combines multiple filters with AND logic", {
  fixture <- .funga_records_fixture()
  .mock_funga_records(fixture)

  result <- funga_records(phytogeographicDomain = "Caatinga",
                          lifeForm = "Saprobio",
                          verbose = FALSE)
  expect_equal(result$taxonName, "Trichoderma harzianum")

  none <- funga_records(phytogeographicDomain = "Amazonia",
                        lifeForm = "Saprobio",
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
                          filename = "hypocreaceae_records",
                          verbose = FALSE)

  out <- file.path(tmp_dir, "hypocreaceae_records.csv")
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
      speciesprofile.txt = data.frame(id = "1", lifeForm = "Saprobio", stringsAsFactors = FALSE)
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
  expect_equal(result$speciesprofile_df$lifeForm, "Saprobio")
})


test_that("funga_records adds the distribution and species profile, joined with ' | '", {
  fixture <- .funga_records_fixture()
  .mock_funga_records(fixture)

  result <- funga_records(verbose = FALSE)
  expect_true(all(c("state", "phytogeographicDomain", "endemism", "establishmentMeans",
                    "lifeForm", "habitat", "vegetationType") %in% names(result)))

  harzianum <- result[result$taxonName == "Trichoderma harzianum", ]
  expect_equal(harzianum$state, "BA | MG")
  expect_equal(harzianum$phytogeographicDomain, "Caatinga | Mata Atlantica")
  expect_equal(harzianum$endemism, "true")          # repeated values appear once
  expect_equal(harzianum$establishmentMeans, "Nativa")
  expect_equal(harzianum$lifeForm, "Saprobio")
  expect_equal(harzianum$vegetationType, "Caatinga (stricto sensu)")

  # A taxon without distribution or profile rows gets NA
  synonym <- result[result$taxonName == "Trichoderma synonym", ]
  expect_true(is.na(synonym$state))
  expect_true(is.na(synonym$lifeForm))
})


test_that("funga_records keeps the taxon's full distribution after an area filter", {
  fixture <- .funga_records_fixture()
  .mock_funga_records(fixture)

  result <- funga_records(state = "Minas Gerais", verbose = FALSE)
  expect_equal(result$taxonName, "Trichoderma harzianum")
  expect_equal(result$state, "BA | MG")
})


test_that(".collapse_by_id sorts values ignoring accents and drops empty ones", {
  out <- .collapse_by_id(id = c(1, 1, 1, 2, 2), value = c("\u00c1rvore", "Erva", "", NA, " "),
                         ids = c(2, 1, 3))
  expect_equal(out, c(NA, "\u00c1rvore | Erva", NA))
})
