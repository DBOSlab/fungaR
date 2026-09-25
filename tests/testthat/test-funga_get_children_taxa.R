.funga_children_fixture <- function() {
  # Real FFB fungi have no standalone FAMILIA-rank taxon record (confirmed
  # empirically against the live dataset): 'family' is only a classification
  # column on genus/species rows, and genus rows link straight up to an
  # ORDEM-rank parent. This fixture mirrors that real structure: Ascomycota
  # (division) -> Hypocreales (order) -> Trichoderma, Xylaria (genera,
  # both tagged family = "Hypocreaceae") -> species.
  data.frame(
    id = as.character(1:8),
    taxonName = c(NA, NA, "Trichoderma", "Xylaria", "Trichoderma harzianum",
                  "Trichoderma viride", "Xylaria hypoxylon", "Xylaria fakesynonym"),
    taxonRank = c("DIVISAO", "ORDEM", "GENERO", "GENERO", "ESPECIE",
                  "ESPECIE", "ESPECIE", "ESPECIE"),
    phylum = c("Ascomycota", "Ascomycota", "Ascomycota", "Ascomycota",
              "Ascomycota", "Ascomycota", "Ascomycota", "Ascomycota"),
    family = c(NA, NA, "Hypocreaceae", "Hypocreaceae", "Hypocreaceae",
              "Hypocreaceae", "Hypocreaceae", "Hypocreaceae"),
    genus = c(NA, NA, "Trichoderma", "Xylaria", "Trichoderma", "Trichoderma", "Xylaria", "Xylaria"),
    order = c(NA, "Hypocreales", "Hypocreales", "Hypocreales", "Hypocreales",
             "Hypocreales", "Hypocreales", "Hypocreales"),
    taxonomicStatus = c("NOME_ACEITO", "NOME_ACEITO", "NOME_ACEITO", "NOME_ACEITO",
                        "NOME_ACEITO", "NOME_ACEITO", "NOME_ACEITO", "SINONIMO"),
    parentNameUsageID = c(NA, "1", "2", "2", "3", "3", "4", "4"),
    stringsAsFactors = FALSE
  )
}

.mock_funga_children <- function(fixture, env = parent.frame()) {
  dwca <- list(dwca_ffb_v393_001_latest = list(data = list(taxon.txt = fixture)))
  testthat::local_mocked_bindings(
    funga_download = function(version, dir, verbose) invisible(NULL),
    funga_parse = function(path, version, verbose) dwca,
    .package = "fungaR",
    .env = env
  )
  # funga_get_children_taxa() re-derives the dwca key via
  # list.files("funga_download") against the real working directory, even
  # though funga_download()/funga_parse() are otherwise mocked above.
  withr::local_dir(withr::local_tempdir(.local_envir = env), .local_envir = env)
  dir.create(file.path("funga_download", names(dwca)), recursive = TRUE, showWarnings = FALSE)
}


test_that("funga_get_children_taxa returns direct species children of a genus", {
  .mock_funga_children(.funga_children_fixture())

  result <- funga_get_children_taxa(taxon_name = "Trichoderma", rank = "genus",
                                    child_rank = "species", verbose = FALSE)
  expect_setequal(result$taxonName, c("Trichoderma harzianum", "Trichoderma viride"))
})


test_that("funga_get_children_taxa returns genera for a family (matched via classification column)", {
  .mock_funga_children(.funga_children_fixture())

  result <- funga_get_children_taxa(taxon_name = "Hypocreaceae", rank = "family",
                                    child_rank = "genus", verbose = FALSE)
  expect_setequal(result$taxonName, c("Trichoderma", "Xylaria"))
})


test_that("funga_get_children_taxa returns genera for an order", {
  .mock_funga_children(.funga_children_fixture())

  result <- funga_get_children_taxa(taxon_name = "Hypocreales", rank = "order",
                                    child_rank = "genus", verbose = FALSE)
  expect_setequal(result$taxonName, c("Trichoderma", "Xylaria"))
})


test_that("funga_get_children_taxa returns orders for a division", {
  .mock_funga_children(.funga_children_fixture())

  result <- funga_get_children_taxa(taxon_name = "Ascomycota", rank = "division",
                                    child_rank = "order", verbose = FALSE)
  expect_equal(nrow(result), 1)
  expect_equal(result$order, "Hypocreales")
})


test_that("funga_get_children_taxa excludes synonyms by default", {
  .mock_funga_children(.funga_children_fixture())

  result <- funga_get_children_taxa(taxon_name = "Xylaria", rank = "genus",
                                    child_rank = "species", verbose = FALSE)
  expect_equal(result$taxonName, "Xylaria hypoxylon")
})


test_that("funga_get_children_taxa includes synonyms when requested", {
  .mock_funga_children(.funga_children_fixture())

  result <- funga_get_children_taxa(taxon_name = "Xylaria", rank = "genus",
                                    child_rank = "species", include_synonyms = TRUE,
                                    verbose = FALSE)
  expect_setequal(result$taxonName, c("Xylaria hypoxylon", "Xylaria fakesynonym"))
})


test_that("funga_get_children_taxa returns all descendant ranks when child_rank is NULL", {
  .mock_funga_children(.funga_children_fixture())

  # Starting from "genus", the recursive descent only needs a single hop
  # (genus -> species), so it returns every species below it.
  result <- funga_get_children_taxa(taxon_name = "Trichoderma", rank = "genus",
                                    child_rank = NULL, verbose = FALSE)
  expect_setequal(result$taxonName, c("Trichoderma harzianum", "Trichoderma viride"))
})


test_that("funga_get_children_taxa returns every family member (genus + species) when child_rank is NULL", {
  .mock_funga_children(.funga_children_fixture())

  # FFB has no standalone family-rank row for Fungi, so with rank = "family"
  # and child_rank = NULL, ALL genus/species/infraspecific rows sharing that
  # family classification are returned (there is no single "family row" to
  # stop the walk at, unlike the recursive-rank case above). The synonym
  # (Xylaria fakesynonym) is excluded by the default include_synonyms = FALSE,
  # leaving 5 of the fixture's 6 non-division/order members.
  result <- funga_get_children_taxa(taxon_name = "Hypocreaceae", rank = "family",
                                    child_rank = NULL, verbose = FALSE)
  expect_equal(nrow(result), 5)
  expect_true(all(c("Trichoderma", "Xylaria", "Trichoderma harzianum",
                    "Xylaria hypoxylon") %in% result$taxonName))
})


test_that("funga_get_children_taxa derives a family summary when child_rank = 'family'", {
  .mock_funga_children(.funga_children_fixture())

  result <- funga_get_children_taxa(taxon_name = "Hypocreales", rank = "order",
                                    child_rank = "family", verbose = FALSE)
  expect_equal(nrow(result), 1)
  expect_equal(result$family, "Hypocreaceae")
  expect_equal(result$n_genera, 2)
})


test_that("funga_get_children_taxa errors when the parent taxon is not found", {
  .mock_funga_children(.funga_children_fixture())

  expect_error(
    funga_get_children_taxa(taxon_name = "Nonexistentaceae", rank = "family",
                            child_rank = "genus", verbose = FALSE),
    "not found in FFB database"
  )
})


test_that("funga_get_children_taxa errors when child_rank is not lower than the parent rank", {
  .mock_funga_children(.funga_children_fixture())

  expect_error(
    funga_get_children_taxa(taxon_name = "Trichoderma", rank = "genus",
                            child_rank = "family", verbose = FALSE),
    "must be lower than parent rank"
  )
})


test_that("funga_get_children_taxa warns and returns an empty data.frame when nothing matches", {
  .mock_funga_children(.funga_children_fixture())

  expect_warning(
    result <- funga_get_children_taxa(taxon_name = "Trichoderma", rank = "genus",
                                      child_rank = "subspecies", verbose = FALSE),
    "No children found"
  )
  expect_equal(nrow(result), 0)
})


test_that("funga_get_children_taxa requires a non-null taxon_name", {
  expect_error(funga_get_children_taxa(taxon_name = NULL), "must be provided")
})
