# ---------------------------------------------------------------------------
# Fixtures: GBIF names of a genus with Brazilian type specimens, their
# backbone names, and the specimens themselves
# ---------------------------------------------------------------------------
.gbif_counts_fixture <- function() {
  data.frame(GBIF_Key = c("1", "2", "3", "4", "5", "6"),
             N_type_specimens = c(3L, 2L, 1L, 4L, 2L, 5L),
             stringsAsFactors = FALSE)
}

.gbif_names_fixture <- function() {
  data.frame(
    GBIF_Key = c("1", "2", "3", "4", "5", "6", "9"),
    Taxon_name = c("Trichoderma nova", "Trichoderma andrade-limae", "Trichoderma vetus",
                   "Trichoderma ferruginea", "Trichoderma alba var. minor", "Trichoderma recens",
                   "Trichoderma unrelated"),
    Authors = c("Anon.", "Lima", "Berg", "DC.", "Berg", "Silva", "X"),
    Rank = c("SPECIES", "SPECIES", "SPECIES", "SPECIES", "VARIETY", "SPECIES", "SPECIES"),
    Taxonomic_status = c("ACCEPTED", "ACCEPTED", "SYNONYM", "SYNONYM", "SYNONYM",
                         "DOUBTFUL", "ACCEPTED"),
    Accepted_name = c("Trichoderma nova", "Trichoderma andrade-limae", "Trichoderma splendens",
                      "Hypocrea florida", "Trichoderma alba", "Trichoderma recens",
                      "Trichoderma unrelated"),
    Accepted_full_name = c("Trichoderma nova Anon.", "Trichoderma andrade-limae Lima",
                           "Trichoderma splendens (Sw.) DC.", "Hypocrea florida DC.",
                           "Trichoderma alba Berg", "Trichoderma recens Silva",
                           "Trichoderma unrelated X"),
    Genus = "Trichoderma",
    GBIF_Published_in = c("Phytotaxa 1: 1 (2026)", NA, NA, NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )
}

.gbif_specimens_fixture <- function(keys) {
  sp <- data.frame(
    GBIF_Key = c("1", "1", "1", "6", "66"),
    Accepted_key = c("1", "1", "1", "6", "6"),
    Species_key = c("1", "1", "1", "6", "6"),
    Specimen_name = c("Trichoderma nova Anon.", "Trichoderma nova Anon.", "Trichoderma nova Anon.",
                      "Trichoderma recens Silva", "Hypocrea recens Berg"),
    Occurrence_ID = c("11", "12", "13", "61", "62"),
    Type_status = c("Holotype", "Isotype", "Isotype", "Type", "Paratype"),
    Typified_name = NA_character_,
    Institution = c("RB", "K", "NY", "SPF", NA),
    Catalog_number = c("00123", "K0001", "NY9", "555", "777"),
    Collector = c("A. Silva", "A. Silva", "A. Silva", "B. Souza", "B. Souza"),
    Year = c("2020", "2020", "2021", "1990", "1990"),
    State = c("Bahia", "Bahia", "Minas Gerais", "Goi\u00e1s", NA),
    stringsAsFactors = FALSE
  )
  # Like GBIF, a query for a key also returns its synonyms' specimens
  sp[sp$GBIF_Key %in% keys | sp$Accepted_key %in% keys, ]
}

.mock_gbif <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    .gbif_genus_key = function(taxon) {
      if (taxon == "Nonexistentia") NULL else
        list(key = 99L, name = paste(taxon, "DC."), kingdom = "Fungi")
    },
    .gbif_brazil_type_counts = function(genus_key, type_status) .gbif_counts_fixture(),
    .gbif_genus_names = function(genus_key) .gbif_names_fixture(),
    .gbif_type_specimens = function(keys, type_status, ...) .gbif_specimens_fixture(keys),
    .package = "fungaR",
    .env = env
  )
}

.mock_gbif_ffb <- function(taxon_names, other_names = character(0), env = parent.frame()) {
  testthat::local_mocked_bindings(
    .ffb_names = function(taxon, version) {
      list(genus = taxon_names, all = c(taxon_names, other_names))
    },
    .package = "fungaR",
    .env = env
  )
}


test_that("gbif_gap flags species with Brazilian type specimens missing from FFB", {
  .mock_gbif()
  .mock_gbif_ffb(character(0))

  result <- gbif_gap(taxon = "Trichoderma", save = FALSE, html_report = FALSE, verbose = FALSE)

  expect_setequal(result$Taxon_name, c("Trichoderma nova", "Trichoderma andrade-limae",
                                       "Trichoderma vetus", "Trichoderma ferruginea",
                                       "Trichoderma recens"))
  expect_true(all(!result$In_FFB))
})


test_that("gbif_gap reports species-level names only", {
  .mock_gbif()
  .mock_gbif_ffb(character(0))

  result <- gbif_gap(taxon = "Trichoderma", save = FALSE, html_report = FALSE, verbose = FALSE)

  expect_false("Trichoderma alba var. minor" %in% result$Taxon_name)
  # A backbone name without Brazilian types is never reported
  expect_false("Trichoderma unrelated" %in% result$Taxon_name)
})


test_that("gbif_gap does not flag a name whose accepted name FFB files under another genus", {
  .mock_gbif()
  .mock_gbif_ffb("Trichoderma splendens", other_names = "Hypocrea florida")

  result <- gbif_gap(taxon = "Trichoderma", save = FALSE, html_report = FALSE, verbose = FALSE)

  expect_false(any(c("Trichoderma vetus", "Trichoderma ferruginea") %in% result$Taxon_name))
})


test_that("gbif_gap treats orthographic variants as the same name", {
  .mock_gbif()
  .mock_gbif_ffb("Trichoderma andradelimae")

  result <- gbif_gap(taxon = "Trichoderma", save = FALSE, html_report = FALSE, verbose = FALSE)

  expect_false("Trichoderma andrade-limae" %in% result$Taxon_name)
})


test_that("gbif_gap summarises each name's Brazilian type specimens", {
  .mock_gbif()
  .mock_gbif_ffb(character(0))

  result <- gbif_gap(taxon = "Trichoderma", save = FALSE, html_report = FALSE, verbose = FALSE)
  nova <- result[result$Taxon_name == "Trichoderma nova", ]

  expect_equal(nova$N_type_specimens, 3L)
  expect_equal(nova$Type_statuses, "Holotype; Isotype")
  expect_equal(nova$Type_specimens, "RB 00123 (Holotype); K K0001 (Isotype); NY NY9 (Isotype)")
  expect_equal(nova$Collectors, "A. Silva")
  expect_equal(nova$Years, "2020-2021")
  expect_equal(nova$States, "Bahia; Minas Gerais")
  expect_equal(nova$GBIF_URL, "https://www.gbif.org/species/1")
  expect_false("GBIF_Types_URL" %in% names(result))

  recens <- result[result$Taxon_name == "Trichoderma recens", ]
  expect_equal(recens$Type_specimens, "SPF 555 (Type); 777 (Paratype)")
  expect_equal(recens$Years, "1990")
})


test_that("gbif_gap assigns types filed under a synonym to the requested name", {
  .mock_gbif()
  .mock_gbif_ffb(character(0))

  result <- gbif_gap(taxon = "Trichoderma", save = FALSE, html_report = FALSE, verbose = FALSE)
  recens <- result[result$Taxon_name == "Trichoderma recens", ]

  expect_equal(recens$Specimen_names, "Trichoderma recens Silva; Hypocrea recens Berg")
})


test_that("gbif_gap leaves specimen columns NA when no specimen could be retrieved", {
  .mock_gbif()
  .mock_gbif_ffb(character(0))

  result <- gbif_gap(taxon = "Trichoderma", save = FALSE, html_report = FALSE, verbose = FALSE)
  vetus <- result[result$Taxon_name == "Trichoderma vetus", ]

  expect_true(is.na(vetus$Type_specimens))
  expect_true(is.na(vetus$Years))
})


test_that("gbif_gap fetches specimens only for names missing from FFB", {
  fetched <- NULL
  .mock_gbif()
  testthat::local_mocked_bindings(
    .gbif_type_specimens = function(keys, type_status, ...) {
      fetched <<- keys
      .gbif_specimens_fixture(keys)
    },
    .package = "fungaR"
  )
  .mock_gbif_ffb(c("Trichoderma nova", "Trichoderma recens"))

  gbif_gap(taxon = "Trichoderma", save = FALSE, html_report = FALSE, verbose = FALSE)

  expect_setequal(fetched, c("2", "3", "4"))
})


test_that("gbif_gap passes a custom type_status on to GBIF", {
  seen <- NULL
  .mock_gbif()
  testthat::local_mocked_bindings(
    .gbif_brazil_type_counts = function(genus_key, type_status) {
      seen <<- type_status
      .gbif_counts_fixture()
    },
    .package = "fungaR"
  )
  .mock_gbif_ffb(character(0))

  gbif_gap(taxon = "Trichoderma", type_status = c("Holotype", "Isotype"),
           save = FALSE, html_report = FALSE, verbose = FALSE)

  expect_equal(seen, c("Holotype", "Isotype"))
})


test_that("gbif_gap returns an empty data.frame when every name is already in FFB", {
  .mock_gbif()
  .mock_gbif_ffb(.gbif_names_fixture()$Taxon_name)

  result <- gbif_gap(taxon = "Trichoderma", save = FALSE, html_report = FALSE, verbose = FALSE)

  expect_equal(nrow(result), 0)
  expect_true("In_FFB" %in% names(result))
})


test_that("gbif_gap stops with a clear message for a genus not in GBIF", {
  .mock_gbif()
  expect_error(gbif_gap(taxon = "Nonexistentia", verbose = FALSE),
               "not found in the GBIF backbone among Fungi")
})


test_that(".gbif_genus_key searches only the Fungi kingdom", {
  queried <- character(0)
  testthat::local_mocked_bindings(
    .get_json = function(url) {
      queried <<- c(queried, sub(".*&kingdom=([^&]+).*", "\\1", url))
      NULL
    },
    .package = "fungaR"
  )

  expect_null(.gbif_genus_key("Nonexistentia"))
  expect_equal(queried, c("Fungi"))
})


test_that("gbif_gap has no kingdom argument", {
  expect_false("kingdom" %in% names(formals(gbif_gap)))
})


test_that("gbif_gap requires a single character taxon", {
  expect_error(gbif_gap(taxon = NULL), "single character string")
  expect_error(gbif_gap(taxon = c("A", "B")), "single character string")
})


test_that("gbif_gap writes a real .xlsx file and HTML report", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  .mock_gbif()
  .mock_gbif_ffb(character(0))
  out_dir <- withr::local_tempdir()

  gbif_gap(taxon = "Trichoderma", save = TRUE, html_report = TRUE, open_report = FALSE,
           dir = out_dir, filename = "gbif_test", verbose = FALSE)

  expect_true(file.exists(file.path(out_dir, "gbif_test.xlsx")))
  expect_true(file.exists(file.path(out_dir, "gbif_test.html")))
})
