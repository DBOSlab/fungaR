# ---------------------------------------------------------------------------
# Fixtures: a miniature FFB (taxa + distribution) and GBIF state counts
# ---------------------------------------------------------------------------
.dist_ffb_fixture <- function() {
  taxon_df <- data.frame(
    id = c("10", "11", "12", "13", "14"),
    taxonName = c("Trichoderma", "Trichoderma harzianum", "Trichoderma bahiense",
                  "Trichoderma pallidiflorum", "Trichoderma harzianum var. minor"),
    genus = "Trichoderma",
    acceptedNameUsageID = c(NA, NA, NA, "11", NA),
    stringsAsFactors = FALSE
  )
  distribution_df <- data.frame(
    id = c("11", "11", "12", "14"),
    locationID = c("BR-BA", "BR-PE", "BR-MG", "BR-CE"),
    phytogeographicDomain = c("Caatinga", "Caatinga", "Cerrado", "Caatinga"),
    stringsAsFactors = FALSE
  )
  list(taxon_df = taxon_df, distribution_df = distribution_df, speciesprofile_df = data.frame())
}

.mock_dist_ffb <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    .funga_prepare_records = function(version, verbose, rm_funga_database) .dist_ffb_fixture(),
    # Reflora and JABOT are queried by default: no archives in tests, unless a
    # test mocks them with herbarium records
    .herbarium_records = function(source, taxon, verbose = TRUE) NULL,
    .package = "fungaR",
    .env = env
  )
}

.mock_dist_gbif <- function(state_counts, found = TRUE, coordinates = NULL,
                            herbaria = list(), env = parent.frame()) {
  testthat::local_mocked_bindings(
    # Reflora and JABOT: the records given in 'herbaria' (by source), else none
    .herbarium_records = function(source, taxon, verbose = TRUE) herbaria[[source]],
    .gbif_fungi_key = function(taxon) {
      if (found) list(key = 1L, name = taxon, kingdom = "Fungi") else NULL
    },
    .gbif_brazil_state_counts = function(taxon_key) {
      list(n_records = sum(state_counts$GBIF_records), state_counts = state_counts,
           raw_states = split(state_counts$State, state_counts$State))
    },
    .gbif_brazil_coordinates = function(taxon_key, max_records = 5000) {
      if (is.null(coordinates)) .empty_coord_records()[0, setdiff(names(.empty_coord_records()),
                                                                  c("Taxon", "Source", "State", "Domain",
                                                                    "New_state_record_candidate",
                                                                    "New_domain_record_candidate"))]
      else coordinates
    },
    .gbif_brazil_state_records = function(taxon_key, raw_states, limit = 50) {
      data.frame(Municipality = "Recife", Locality = "Dois Irmaos", RecordedBy = "A. Silva",
                 RecordNumber = "12", EventDate = "2001-05-01", Institution = "UFP",
                 CatalogNumber = "999", Identified_as = "Trichoderma harzianum",
                 RecordLink = "https://www.gbif.org/occurrence/1", stringsAsFactors = FALSE)
    },
    .package = "fungaR",
    .env = env
  )
}

.gbif_counts <- function(states, n) {
  data.frame(State = states, GBIF_records = as.integer(n), stringsAsFactors = FALSE)
}


# ---------------------------------------------------------------------------
# State normalisation
# ---------------------------------------------------------------------------
test_that(".normalize_br_state normalises the messy state values found in GBIF", {
  expect_equal(.normalize_br_state(c("Go", "Df", "Est. do Bahia", "Bras\u00edlia", "Paraiba",
                                     "rs", "Estado de S\u00e3o Paulo", "State of Minas Gerais")),
               c("Goi\u00e1s", "Distrito Federal", "Bahia", "Distrito Federal",
                 "Para\u00edba", "Rio Grande do Sul", "S\u00e3o Paulo", "Minas Gerais"))
})


test_that(".normalize_br_state returns NA for values that are not Brazilian states", {
  expect_equal(.normalize_br_state(c("Paraguay", "Buenos Aires", NA, "")),
               rep(NA_character_, 4))
  # "Para" is the state of Par\u00e1, not a prefix match of Paran\u00e1 or Para\u00edba
  expect_equal(.normalize_br_state("Para"), "Par\u00e1")
})


# ---------------------------------------------------------------------------
# FFB's official states
# ---------------------------------------------------------------------------
test_that(".ffb_taxon_states includes infraspecific taxa for a species", {
  f <- .dist_ffb_fixture()
  res <- .ffb_taxon_states("Trichoderma harzianum", f$taxon_df, f$distribution_df)
  expect_setequal(res$states, c("Bahia", "Pernambuco", "Cear\u00e1"))
})


test_that(".ffb_taxon_states combines all species of a genus", {
  f <- .dist_ffb_fixture()
  res <- .ffb_taxon_states("Trichoderma", f$taxon_df, f$distribution_df)
  expect_setequal(res$states, c("Bahia", "Pernambuco", "Cear\u00e1", "Minas Gerais"))
})


test_that(".ffb_taxon_states uses the accepted name of an FFB synonym", {
  f <- .dist_ffb_fixture()
  res <- .ffb_taxon_states("Trichoderma pallidiflorum", f$taxon_df, f$distribution_df)
  expect_equal(res$name, "Trichoderma harzianum")
  expect_setequal(res$states, c("Bahia", "Pernambuco", "Cear\u00e1"))
})


test_that(".ffb_taxon_states reports a taxon absent from FFB", {
  f <- .dist_ffb_fixture()
  res <- .ffb_taxon_states("Trichoderma novum", f$taxon_df, f$distribution_df)
  expect_false(res$found)
  expect_length(res$states, 0)
})


# ---------------------------------------------------------------------------
# funga_distribution_gap()
# ---------------------------------------------------------------------------
test_that("funga_distribution_gap flags a GBIF state not in FFB's distribution", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts(c("Bahia", "Para\u00edba"), c(5, 2)))

  result <- funga_distribution_gap(taxon = "Trichoderma harzianum", save = FALSE,
                                   html_report = FALSE, verbose = FALSE)$states

  pb <- result[result$State == "Para\u00edba", ]
  expect_true(pb$New_state_record_candidate)
  expect_false(pb$In_FFB_distribution)
  expect_equal(pb$GBIF_records, 2L)
  expect_false(result$New_state_record_candidate[result$State == "Bahia"])
})


test_that("funga_distribution_gap keeps FFB states without records, not as candidates", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts("Bahia", 5))

  result <- funga_distribution_gap(taxon = "Trichoderma harzianum", save = FALSE,
                                   html_report = FALSE, verbose = FALSE)$states

  pe <- result[result$State == "Pernambuco", ]
  expect_true(pe$In_FFB_distribution)
  expect_equal(pe$GBIF_records, 0L)
  expect_false(pe$New_state_record_candidate)
})


test_that("funga_distribution_gap compares a genus against all its species' states", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts(c("Minas Gerais", "Goi\u00e1s"), c(3, 1)))

  result <- funga_distribution_gap(taxon = "Trichoderma", save = FALSE,
                                   html_report = FALSE, verbose = FALSE)$states

  expect_false(result$New_state_record_candidate[result$State == "Minas Gerais"])
  expect_true(result$New_state_record_candidate[result$State == "Goi\u00e1s"])
})


test_that("funga_distribution_gap restricts the check to the requested states", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts(c("Bahia", "Para\u00edba", "Goi\u00e1s"), c(5, 2, 1)))

  result <- funga_distribution_gap(taxon = "Trichoderma harzianum", state = c("PB", "Goias"),
                                   save = FALSE, html_report = FALSE, verbose = FALSE)$states

  expect_setequal(result$State, c("Para\u00edba", "Goi\u00e1s"))
})


test_that("funga_distribution_gap rejects an unrecognised state", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts("Bahia", 5))

  expect_error(funga_distribution_gap(taxon = "Trichoderma harzianum", state = "Atlantis",
                                      save = FALSE, html_report = FALSE, verbose = FALSE),
               "Unrecognised Brazilian state")
})


test_that("funga_distribution_gap still reports FFB states when GBIF has no match", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts(character(0), integer(0)), found = FALSE)

  result <- funga_distribution_gap(taxon = "Trichoderma harzianum", save = FALSE,
                                   html_report = FALSE, verbose = FALSE)$states

  expect_setequal(result$State, c("Bahia", "Pernambuco", "Cear\u00e1"))
  expect_false(any(result$New_state_record_candidate))
})


test_that("funga_distribution_gap skips speciesLink without an API key", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts("Bahia", 5))
  called <- FALSE
  testthat::local_mocked_bindings(
    .splink_brazil_state_counts = function(taxon, key, ...) {
      called <<- TRUE
      data.frame(State = character(0), speciesLink_records = integer(0))
    },
    .package = "fungaR"
  )

  expect_message(
    result <- funga_distribution_gap(taxon = "Trichoderma harzianum",
                                     sources = c("gbif", "speciesLink"), specieslink_key = "",
                                     save = FALSE, html_report = FALSE, verbose = TRUE)$states,
    "No speciesLink API key"
  )
  expect_false(called)
  expect_true(all(result$speciesLink_records == 0L))
})


test_that("funga_distribution_gap adds speciesLink evidence when a key is given", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts("Bahia", 5))
  testthat::local_mocked_bindings(
    .splink_brazil_state_counts = function(taxon, key, ...) {
      data.frame(State = "Sergipe", speciesLink_records = 4L, stringsAsFactors = FALSE)
    },
    .package = "fungaR"
  )

  result <- funga_distribution_gap(taxon = "Trichoderma harzianum",
                                   sources = c("gbif", "speciesLink"), specieslink_key = "k",
                                   save = FALSE, html_report = FALSE, verbose = FALSE)$states

  se <- result[result$State == "Sergipe", ]
  expect_equal(se$speciesLink_records, 4L)
  expect_equal(se$GBIF_records, 0L)
  expect_true(se$New_state_record_candidate)
})


test_that("funga_distribution_gap explains when FFB lists no state for the taxon", {
  f <- .dist_ffb_fixture()
  f$distribution_df$locationID[f$distribution_df$id == "12"] <- NA
  testthat::local_mocked_bindings(
    .funga_prepare_records = function(version, verbose, rm_funga_database) f,
    .package = "fungaR"
  )
  .mock_dist_gbif(.gbif_counts("Sergipe", 3))

  expect_message(
    result <- funga_distribution_gap(taxon = "Trichoderma bahiense", save = FALSE,
                                     html_report = FALSE, verbose = TRUE)$states,
    "FFB lists no Brazilian state"
  )
  expect_true(result$New_state_record_candidate[result$State == "Sergipe"])
})


test_that("funga_distribution_gap requires a character taxon and valid sources", {
  expect_error(funga_distribution_gap(taxon = NULL), "character vector")
  expect_error(funga_distribution_gap(taxon = 1), "character vector")
  expect_error(funga_distribution_gap(taxon = c("A", "")), "character vector")
  expect_error(funga_distribution_gap(taxon = "A", sources = "inaturalist"), "should be one of")
})


test_that("funga_distribution_gap writes a real .xlsx file and HTML report", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts(c("Bahia", "Para\u00edba"), c(5, 2)))
  out_dir <- withr::local_tempdir()

  funga_distribution_gap(taxon = "Trichoderma harzianum", save = TRUE, html_report = TRUE,
                         open_report = FALSE, dir = out_dir, verbose = FALSE)

  stem <- file.path(out_dir, "funga_distribution_gap_Trichoderma_harzianum")
  expect_true(file.exists(paste0(stem, ".xlsx")))
  expect_true(file.exists(paste0(stem, ".html")))
})


# ---------------------------------------------------------------------------
# Phytogeographic domains
# ---------------------------------------------------------------------------
# Georeferenced GBIF records: Caatinga of Bahia (already in FFB), Cerrado near
# Brasilia and Mata Atlantica of Rio de Janeiro (candidates), and one offshore
.coords_fixture <- function() {
  data.frame(
    Latitude = c(-9.5, -15.8, -22.9, -5.0),
    Longitude = c(-40.5, -47.9, -43.2, -30.0),
    StateProvince = c("Bahia", "Distrito Federal", "Rio de Janeiro", NA),
    Municipality = c("Jeremoabo", "Brasilia", "Rio de Janeiro", NA),
    Locality = NA_character_, RecordedBy = c("A", "B", "C", "D"),
    RecordNumber = c("1", "2", "3", "4"), EventDate = "2001-01-01",
    Institution = "RB", CatalogNumber = c("11", "12", "13", "14"),
    Identified_as = "Trichoderma harzianum",
    RecordLink = paste0("https://www.gbif.org/occurrence/", 1:4),
    stringsAsFactors = FALSE)
}


test_that("funga_distribution_gap returns state, domain, and record tables", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts("Bahia", 5))

  result <- funga_distribution_gap(taxon = "Trichoderma harzianum", save = FALSE,
                                   html_report = FALSE, verbose = FALSE)
  expect_named(result, c("states", "domains", "records"))
  expect_s3_class(result$states, "data.frame")
})


test_that("funga_distribution_gap flags georeferenced records in domains not listed by FFB", {
  skip_if_not_installed("sf")
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts(c("Bahia", "Distrito Federal", "Rio de Janeiro"), c(1, 1, 1)),
                  coordinates = .coords_fixture())

  result <- funga_distribution_gap(taxon = "Trichoderma harzianum", save = FALSE,
                                   html_report = FALSE, verbose = FALSE)

  dom <- result$domains
  expect_setequal(dom$Domain, c("Caatinga", "Cerrado", "Mata Atl\u00e2ntica"))
  expect_true(dom$In_FFB_distribution[dom$Domain == "Caatinga"])
  expect_true(dom$New_domain_record_candidate[dom$Domain == "Cerrado"])
  expect_true(dom$New_domain_record_candidate[dom$Domain == "Mata Atl\u00e2ntica"])
  expect_equal(dom$Georeferenced_records[dom$Domain == "Cerrado"], 1L)

  rec <- result$records
  expect_equal(nrow(rec), 4)
  expect_equal(rec$Domain, c("Caatinga", "Cerrado", "Mata Atl\u00e2ntica", NA))
  expect_equal(rec$New_domain_record_candidate, c(FALSE, TRUE, TRUE, FALSE))
  expect_equal(rec$New_state_record_candidate, c(FALSE, TRUE, TRUE, FALSE))
})


test_that("funga_distribution_gap never flags an offshore record as a new domain", {
  skip_if_not_installed("sf")
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts("Bahia", 1), coordinates = .coords_fixture()[4, ])

  result <- funga_distribution_gap(taxon = "Trichoderma harzianum", save = FALSE,
                                   html_report = FALSE, verbose = FALSE)
  expect_true(is.na(result$records$Domain))
  expect_false(any(result$domains$New_domain_record_candidate))
})


test_that("funga_distribution_gap skips the domain analysis when domains = FALSE", {
  called <- FALSE
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts("Bahia", 5))
  testthat::local_mocked_bindings(
    .gbif_brazil_coordinates = function(taxon_key, max_records = 5000) {
      called <<- TRUE
      .coords_fixture()
    },
    .package = "fungaR"
  )

  result <- funga_distribution_gap(taxon = "Trichoderma harzianum", domains = FALSE,
                                   save = FALSE, html_report = FALSE, verbose = FALSE)
  expect_false(called)
  expect_equal(nrow(result$domains), 0)
  expect_equal(nrow(result$records), 0)
})


test_that("funga_distribution_gap passes max_coordinates on to GBIF", {
  seen <- NULL
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts("Bahia", 5))
  testthat::local_mocked_bindings(
    .gbif_brazil_coordinates = function(taxon_key, max_records = 5000) {
      seen <<- max_records
      .coords_fixture()[0, ]
    },
    .package = "fungaR"
  )
  funga_distribution_gap(taxon = "Trichoderma harzianum", max_coordinates = 900,
                         save = FALSE, html_report = FALSE, verbose = FALSE)
  expect_equal(seen, 900)
})


test_that(".assign_br_domain places coordinates in the six domains, NA outside", {
  skip_if_not_installed("sf")
  expect_equal(.assign_br_domain(c(-60, -40.5, -47.9, -43.2, -53, -57, -30),
                                 c(-3, -9.5, -15.8, -22.9, -30, -18, -5)),
               c("Amaz\u00f4nia", "Caatinga", "Cerrado", "Mata Atl\u00e2ntica", "Pampa",
                 "Pantanal", NA))
})


test_that(".ffb_taxon_states also returns the taxon's FFB domains", {
  f <- .dist_ffb_fixture()
  expect_equal(.ffb_taxon_states("Trichoderma", f$taxon_df, f$distribution_df)$domains,
               c("Caatinga", "Cerrado"))
})


test_that("funga_distribution_gap saves the three tables as spreadsheet sheets", {
  skip_if_not_installed("sf")
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts("Bahia", 1), coordinates = .coords_fixture())
  out_dir <- withr::local_tempdir()

  funga_distribution_gap(taxon = "Trichoderma harzianum", save = TRUE, html_report = FALSE,
                         dir = out_dir, verbose = FALSE)
  expect_equal(openxlsx::getSheetNames(file.path(
    out_dir, "funga_distribution_gap_Trichoderma_harzianum.xlsx")),
               c("states", "domains", "records"))
})


test_that("funga_distribution_gap renders the report with the domain map", {
  skip_if_not_installed("sf")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("DT")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts(c("Bahia", "Rio de Janeiro"), c(1, 1)),
                  coordinates = .coords_fixture())
  out_dir <- withr::local_tempdir()

  expect_no_warning(
    funga_distribution_gap(taxon = "Trichoderma harzianum", save = FALSE, html_report = TRUE,
                           open_report = FALSE, dir = out_dir, verbose = FALSE))
  html <- paste(readLines(file.path(out_dir,
                                    "funga_distribution_gap_Trichoderma_harzianum.html"),
                          warn = FALSE), collapse = "")
  expect_match(html, "Map of Georeferenced Records", fixed = TRUE)
  expect_match(html, "leaflet", fixed = TRUE)
})


# ---------------------------------------------------------------------------
# Reflora and JABOT herbarium specimens
# ---------------------------------------------------------------------------
# Raw Darwin Core records, as returned by refloraR/jabotR, standardised by
# .std_herbarium_records() (with barRoso::std_place() and std_types())
.herbarium_fixture <- function() {
  reflora <- data.frame(
    collectionCode = c("ALCB", "HUEFS", "LPB"), catalogNumber = c("1", "2", "3"),
    genus = "Trichoderma", specificEpithet = "harzianum",
    scientificName = "Trichoderma harzianum Rifai",
    country = c("Brasil", "Brasil", "Bolivia"),
    stateProvince = c("BA", "Piaui", "Santa Cruz"),
    municipality = NA_character_, locality = NA_character_,
    decimalLatitude = NA, decimalLongitude = NA,
    recordedBy = "D. Cardoso", recordNumber = c("10", "11", "12"), year = "2010",
    typeStatus = NA_character_,
    bibliographicCitation = paste0("Reflora Virtual Herbarium, available at: ",
                                   "https://reflora.jbrj.gov.br/r?codigoBarra=", 1:3),
    stringsAsFactors = FALSE)
  jabot <- data.frame(
    collectionCode = "RB", catalogNumber = "99",
    genus = "Trichoderma", specificEpithet = "harzianum",
    scientificName = "Trichoderma harzianum", country = "Brazil",
    stateProvince = "Sergipe", municipality = "Aracaju", locality = NA_character_,
    decimalLatitude = -10.9, decimalLongitude = -37.1,
    recordedBy = "L.P. Queiroz", recordNumber = "5", year = "1999", typeStatus = NA_character_,
    stringsAsFactors = FALSE)
  list(reflora = .std_herbarium_records(reflora, "Reflora"),
       jabot = .std_herbarium_records(jabot, "JABOT"))
}


test_that(".std_herbarium_records standardises places, links, and names", {
  h <- .herbarium_fixture()$reflora
  expect_equal(h$Country, c("Brazil", "Brazil", "Bolivia"))
  expect_equal(h$State[1:2], c("Bahia", "Piau\u00ed"))
  expect_equal(h$Taxon_name, rep("Trichoderma harzianum", 3))
  expect_equal(h$Institution, c("ALCB", "HUEFS", "LPB"))
  expect_equal(h$RecordLink[1], "https://reflora.jbrj.gov.br/r?codigoBarra=1")
})


test_that("funga_distribution_gap combines GBIF, Reflora, and JABOT by default", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts("Bahia", 5), herbaria = .herbarium_fixture())

  result <- funga_distribution_gap(taxon = "Trichoderma harzianum", save = FALSE,
                                   html_report = FALSE, verbose = FALSE)
  st <- result$states
  expect_true(all(c("GBIF_records", "Reflora_records", "Jabot_records") %in% names(st)))
  # Piaui only in Reflora, Sergipe only in JABOT: both are candidates
  expect_setequal(st$State[st$New_state_record_candidate],
                  c("Piau\u00ed", "Sergipe"))
  expect_equal(st$Reflora_records[st$State == "Bahia"], 1L)
  expect_equal(st$Jabot_records[st$State == "Sergipe"], 1L)
  # The Bolivian specimen is not counted
  expect_equal(sum(st$Reflora_records), 2L)
  # JABOT's georeferenced specimen is placed in a domain
  expect_true("JABOT" %in% result$records$Source)
})


test_that("funga_distribution_gap queries only the requested herbaria", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts("Bahia", 5), herbaria = .herbarium_fixture())

  st <- funga_distribution_gap(taxon = "Trichoderma harzianum", sources = "gbif",
                               save = FALSE, html_report = FALSE, verbose = FALSE)$states
  expect_false(any(c("Reflora_records", "Jabot_records") %in% names(st)))

  st <- funga_distribution_gap(taxon = "Trichoderma harzianum",
                               sources = c("gbif", "jabot"),
                               save = FALSE, html_report = FALSE, verbose = FALSE)$states
  expect_true("Jabot_records" %in% names(st))
  expect_false("Reflora_records" %in% names(st))
})


test_that("keep_downloads = FALSE removes the Reflora and JABOT download folders", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts("Bahia", 5))
  withr::local_dir(withr::local_tempdir())
  run <- function(...) {
    dir.create("reflora_download", showWarnings = FALSE)
    dir.create("jabot_download", showWarnings = FALSE)
    funga_distribution_gap(taxon = "Trichoderma harzianum", save = FALSE,
                           html_report = FALSE, verbose = FALSE, ...)
  }

  run()
  expect_true(all(dir.exists(c("reflora_download", "jabot_download"))))

  run(keep_downloads = FALSE)
  expect_false(any(dir.exists(c("reflora_download", "jabot_download"))))

  # Only the folders of the queried sources are removed
  run(sources = c("gbif", "jabot"), keep_downloads = FALSE)
  expect_true(dir.exists("reflora_download"))
  expect_false(dir.exists("jabot_download"))
})


# ---------------------------------------------------------------------------
# Several taxa in one call
# ---------------------------------------------------------------------------
test_that("several taxa: herbaria are queried once and results are stacked", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts("Bahia", 1))
  calls <- list()
  h <- .herbarium_fixture()
  testthat::local_mocked_bindings(
    .herbarium_records = function(source, taxon, verbose = TRUE) {
      calls[[length(calls) + 1]] <<- taxon
      h[[source]]
    },
    .package = "fungaR"
  )
  out_dir <- withr::local_tempdir()
  res <- funga_distribution_gap(taxon = c("Trichoderma harzianum", "Trichoderma bahiense",
                                          "Trichoderma harzianum"),
                                domains = FALSE, save = TRUE, html_report = FALSE,
                                dir = out_dir, verbose = FALSE)

  # one query per herbarium, with all the (unique) taxa
  expect_length(calls, 2)
  expect_equal(calls[[1]], c("Trichoderma harzianum", "Trichoderma bahiense"))
  # both taxa in one table, each with its own FFB states
  expect_setequal(unique(res$states$Taxon), c("Trichoderma harzianum",
                                               "Trichoderma bahiense"))
  bah <- res$states[res$states$Taxon == "Trichoderma bahiense", ]
  expect_true(bah$In_FFB_distribution[bah$State == "Minas Gerais"])
  # herbarium specimens of harzianum are not counted for bahiense
  expect_false("Piau\u00ed" %in% bah$State[bah$Reflora_records > 0])
  # one spreadsheet per taxon, named after it
  expect_setequal(list.files(out_dir),
                  c("funga_distribution_gap_Trichoderma_harzianum.xlsx",
                    "funga_distribution_gap_Trichoderma_bahiense.xlsx"))
})


test_that("several taxa: a failing taxon is skipped, not fatal", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts("Bahia", 1))
  testthat::local_mocked_bindings(
    .distribution_gap_taxon = function(taxon, ...) {
      if (taxon == "Bad name") stop("boom")
      list(states = data.frame(Taxon = taxon, State = "Bahia"),
           domains = .empty_domain_table(), records = .empty_coord_records())
    },
    .package = "fungaR"
  )
  expect_message(
    res <- funga_distribution_gap(taxon = c("Bad name", "Trichoderma harzianum"),
                                  save = FALSE, html_report = FALSE),
    "Skipping 'Bad name': boom")
  expect_equal(res$states$Taxon, "Trichoderma harzianum")
  # with a single taxon, the error is not swallowed
  expect_error(funga_distribution_gap(taxon = "Bad name", save = FALSE, html_report = FALSE,
                                      verbose = FALSE), "boom")
})


test_that(".herbarium_subset keeps a genus or a species, and .rbind_fill fills columns", {
  recs <- data.frame(Taxon_name = c("Trichoderma harzianum", NA, "Xylaria polymorpha"),
                     Identified_as = c("Trichoderma harzianum Rifai",
                                       "Trichoderma sp.", "Xylaria polymorpha"),
                     stringsAsFactors = FALSE)
  expect_equal(nrow(.herbarium_subset(recs, "Trichoderma")), 2)
  expect_equal(nrow(.herbarium_subset(recs, "trichoderma harzianum")), 1)
  expect_null(.herbarium_subset(NULL, "Trichoderma"))

  out <- .rbind_fill(list(data.frame(a = 1, b = "x"), NULL, data.frame(a = 2, c = TRUE)))
  expect_equal(names(out), c("a", "b", "c"))
  expect_equal(out$b, c("x", NA))
  expect_equal(out$c, c(NA, TRUE))
})
