.mycobank_mock_dir <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  mb <- data.frame(
    ID = 1:5,
    `Taxon name` = c("Trichoderma harzianum", "Trichoderma viride",
                     "Trichoderma newspeciesii", "Trichoderma oldsynonym",
                     "Trichoderma notaspecies"),
    Authors = c("Rifai", "Pers.", "A.Author", "B.Author", "C.Author"),
    Rank = c("sp.", "sp.", "sp.", "sp.", "gen."),
    `Name status` = c("Legitimate", "Legitimate", "Legitimate", "Legitimate", "Legitimate"),
    `MycoBank #` = c(100, 101, 102, 103, 104),
    `Hyperlink to MB` = paste0("https://www.mycobank.org/page/Name details page/", 100:104),
    Classification = rep("Fungi, Dikarya, Ascomycota, Pezizomycotina, Sordariomycetes, Hypocreomycetidae, Hypocreales, Hypocreaceae, Trichoderma", 5),
    `Current MycoBank #` = c(100, 101, 102, 100, 104),
    `Current name` = c(NA, NA, NA, "Trichoderma harzianum", NA),
    Synonymy = NA_character_,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  openxlsx::write.xlsx(mb, file.path(dir, "MBList.xlsx"), sheetName = "Sheet1")
  dir
}

.mock_funga_mycobank_ffb <- function(taxon_names, env = parent.frame()) {
  testthat::local_mocked_bindings(
    funga_get_children_taxa = function(taxon_name, rank, child_rank, include_synonyms, version, verbose) {
      data.frame(taxonName = taxon_names, stringsAsFactors = FALSE)
    },
    .package = "fungaR",
    .env = env
  )
}


test_that("funga_mycobank_gap flags MycoBank species missing from FFB", {
  mb_dir <- .mycobank_mock_dir()
  .mock_funga_mycobank_ffb(c("Trichoderma harzianum"))

  result <- funga_mycobank_gap(taxon = "Trichoderma", rank = "genus",
                               check_locality = FALSE, save = FALSE, html_report = FALSE,
                               mycobank_dir = mb_dir, verbose = FALSE)

  expect_setequal(result$Taxon_name, c("Trichoderma viride", "Trichoderma newspeciesii"))
  expect_true(all(!result$In_FFB))
})


test_that("funga_mycobank_gap excludes MycoBank synonyms whose accepted name is already in FFB", {
  mb_dir <- .mycobank_mock_dir()
  # "Trichoderma oldsynonym"'s Current name is "Trichoderma harzianum", already in FFB
  .mock_funga_mycobank_ffb(c("Trichoderma harzianum"))

  result <- funga_mycobank_gap(taxon = "Trichoderma", rank = "genus",
                               check_locality = FALSE, save = FALSE, html_report = FALSE,
                               mycobank_dir = mb_dir, verbose = FALSE)

  expect_false("Trichoderma oldsynonym" %in% result$Taxon_name)
})


test_that("funga_mycobank_gap does not flag a name as missing when its raw MycoBank name matches FFB even if MycoBank's Current name points to a different genus", {
  # Regression test: MycoBank and FFB can disagree on which genus a name
  # currently belongs to (e.g. MycoBank treats "Phellinotus neoaridus" as a
  # synonym of "Fomitiporella neoarida", while FFB accepts it in Phellinotus).
  # Checking only the effective/Current name would wrongly flag such a name
  # as missing even though FFB's own accepted name matches MycoBank's raw
  # name exactly.
  dir <- withr::local_tempdir()
  mb <- data.frame(
    `Taxon name` = c("Phellinotus neoaridus", "Phellinotus badius"),
    Authors = c("A.Author", "B.Author"),
    Rank = c("sp.", "sp."),
    `Name status` = c("Legitimate", "Legitimate"),
    `MycoBank #` = c(805901, 840993),
    `Hyperlink to MB` = c("https://www.mycobank.org/page/Name details page/805901",
                         "https://www.mycobank.org/page/Name details page/840993"),
    Classification = rep("Fungi, Dikarya, Basidiomycota, Agaricomycetes, Hymenochaetales, Hymenochaetaceae, Phellinotus", 2),
    `Current MycoBank #` = c(700001, 840993),
    `Current name` = c("Fomitiporella neoarida", NA),
    Synonymy = NA_character_,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  openxlsx::write.xlsx(mb, file.path(dir, "MBList.xlsx"), sheetName = "Sheet1")

  .mock_funga_mycobank_ffb(c("Phellinotus neoaridus"))

  result <- funga_mycobank_gap(taxon = "Phellinotus", rank = "genus",
                               check_locality = FALSE, save = FALSE, html_report = FALSE,
                               mycobank_dir = dir, verbose = FALSE)

  expect_false("Phellinotus neoaridus" %in% result$Taxon_name)
  expect_true("Phellinotus badius" %in% result$Taxon_name)
})


test_that("funga_mycobank_gap returns an empty data.frame (not an error) when every MycoBank candidate is already in FFB", {
  # Regression test: when every MycoBank species-level name for the genus/order
  # is already in FFB, 'missing' has 0 rows. Building the final result data.frame
  # used to include 'In_FFB = FALSE' as a bare length-1 scalar, which errors
  # when combined with 0-length columns ("arguments imply differing number of
  # rows: 0, 1") instead of being recycled down to 0 rows.
  mb_dir <- .mycobank_mock_dir()
  .mock_funga_mycobank_ffb(c("Trichoderma harzianum", "Trichoderma viride",
                             "Trichoderma newspeciesii"))

  result <- funga_mycobank_gap(taxon = "Trichoderma", rank = "genus",
                               check_locality = FALSE, save = FALSE, html_report = FALSE,
                               mycobank_dir = mb_dir, verbose = FALSE)

  expect_equal(nrow(result), 0)
  expect_true("In_FFB" %in% names(result))
})


test_that("funga_mycobank_gap excludes non-species ranks (e.g. the genus entry itself)", {
  mb_dir <- .mycobank_mock_dir()
  .mock_funga_mycobank_ffb(character(0))

  result <- funga_mycobank_gap(taxon = "Trichoderma", rank = "genus",
                               check_locality = FALSE, save = FALSE, html_report = FALSE,
                               mycobank_dir = mb_dir, verbose = FALSE)

  expect_false("Trichoderma" %in% result$Taxon_name)
})


test_that("funga_mycobank_gap treats an unmatched FFB taxon as an empty known-species set", {
  mb_dir <- .mycobank_mock_dir()
  testthat::local_mocked_bindings(
    funga_get_children_taxa = function(...) stop("Taxon 'Trichoderma' with rank 'genus' not found in FFB database."),
    .package = "fungaR"
  )

  result <- funga_mycobank_gap(taxon = "Trichoderma", rank = "genus",
                               check_locality = FALSE, save = FALSE, html_report = FALSE,
                               mycobank_dir = mb_dir, verbose = FALSE)

  expect_setequal(result$Taxon_name, c("Trichoderma harzianum", "Trichoderma viride",
                                       "Trichoderma newspeciesii"))
})


.mock_mycobank_locality_session <- function(env = parent.frame()) {
  fake_session <- structure(list(close = function(...) invisible(NULL)), class = "fake_session")
  testthat::local_mocked_bindings(
    ChromoteSession = list(new = function(...) fake_session),
    .package = "chromote",
    .env = env
  )
  testthat::local_mocked_bindings(
    .mycobank_page_details = function(session, url, taxon_name = NULL) {
      # Brazilian locality for the candidate whose MycoBank URL ends in
      # "101" (Trichoderma viride in the fixture); non-Brazilian otherwise.
      if (grepl("101$", url)) {
        list(locality = "Brazil, Bahia", substrate = "on dead wood",
            etymology = "in reference to its color", name_type = "Basionym",
            type_specimen = "URM 12345 holotype", collector = "Someone",
            protolog = "Someone 2020. Journal 1:1-10")
      } else {
        list(locality = "Australia", substrate = "on living Eucalyptus",
            etymology = NA_character_, name_type = NA_character_,
            type_specimen = NA_character_, collector = NA_character_,
            protolog = NA_character_)
      }
    },
    .package = "fungaR",
    .env = env
  )
}


test_that("funga_mycobank_gap flags MycoBank_Brazil_Evidence from each name's own locality", {
  skip_if_not_installed("chromote")

  mb_dir <- .mycobank_mock_dir()
  .mock_funga_mycobank_ffb(c("Trichoderma harzianum"))
  .mock_mycobank_locality_session()

  result <- funga_mycobank_gap(taxon = "Trichoderma", rank = "genus",
                               check_locality = TRUE, require_brazil_evidence = FALSE,
                               save = FALSE, html_report = FALSE,
                               mycobank_dir = mb_dir, verbose = FALSE)

  expect_true("MycoBank_Brazil_Evidence" %in% names(result))
  viride_row <- result[result$Taxon_name == "Trichoderma viride", ]
  expect_true(viride_row$MycoBank_Brazil_Evidence)
  expect_equal(viride_row$MycoBank_Locality, "Brazil, Bahia")

  newsp_row <- result[result$Taxon_name == "Trichoderma newspeciesii", ]
  expect_false(newsp_row$MycoBank_Brazil_Evidence)
})


test_that("funga_mycobank_gap filters to only Brazil-evidence candidates by default", {
  skip_if_not_installed("chromote")

  mb_dir <- .mycobank_mock_dir()
  .mock_funga_mycobank_ffb(c("Trichoderma harzianum"))
  .mock_mycobank_locality_session()

  result <- funga_mycobank_gap(taxon = "Trichoderma", rank = "genus",
                               check_locality = TRUE, save = FALSE, html_report = FALSE,
                               mycobank_dir = mb_dir, verbose = FALSE)

  # Only "Trichoderma viride" (Brazilian locality per the mock) survives the
  # default require_brazil_evidence = TRUE filtering; "Trichoderma
  # newspeciesii" (Australia) is dropped entirely, not just flagged FALSE.
  expect_equal(nrow(result), 1)
  expect_equal(result$Taxon_name, "Trichoderma viride")
  expect_true(result$MycoBank_Brazil_Evidence)
})


test_that("funga_mycobank_gap ignores require_brazil_evidence when check_locality = FALSE", {
  mb_dir <- .mycobank_mock_dir()
  .mock_funga_mycobank_ffb(c("Trichoderma harzianum"))

  result <- funga_mycobank_gap(taxon = "Trichoderma", rank = "genus",
                               check_locality = FALSE, require_brazil_evidence = TRUE,
                               save = FALSE, html_report = FALSE,
                               mycobank_dir = mb_dir, verbose = FALSE)

  expect_setequal(result$Taxon_name, c("Trichoderma viride", "Trichoderma newspeciesii"))
})


test_that("funga_mycobank_gap requires a single character taxon", {
  expect_error(funga_mycobank_gap(taxon = NULL), "single character string")
  expect_error(funga_mycobank_gap(taxon = c("A", "B")), "single character string")
})


test_that("funga_mycobank_gap renders a real HTML report when html_report = TRUE", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  mb_dir <- .mycobank_mock_dir()
  .mock_funga_mycobank_ffb(c("Trichoderma harzianum"))

  out_dir <- withr::local_tempdir()

  result <- funga_mycobank_gap(taxon = "Trichoderma", rank = "genus",
                               check_locality = FALSE, save = FALSE, html_report = TRUE,
                               open_report = FALSE, mycobank_dir = mb_dir,
                               dir = out_dir, filename = "report_test", verbose = FALSE)

  expect_true(file.exists(file.path(out_dir, "report_test.html")))
})


test_that("funga_mycobank_gap reuses a previously downloaded MycoBank list", {
  mb_dir <- .mycobank_mock_dir()
  .mock_funga_mycobank_ffb(character(0))

  testthat::local_mocked_bindings(
    download.file = function(...) stop("should not re-download when cache exists"),
    .package = "utils"
  )

  expect_no_error(
    funga_mycobank_gap(taxon = "Trichoderma", rank = "genus", check_locality = FALSE,
                       save = FALSE, html_report = FALSE, mycobank_dir = mb_dir, verbose = FALSE)
  )
})


test_that("funga_mycobank_gap writes a real .xlsx file when save = TRUE", {
  mb_dir <- .mycobank_mock_dir()
  .mock_funga_mycobank_ffb(c("Trichoderma harzianum"))
  out_dir <- withr::local_tempdir()

  result <- funga_mycobank_gap(taxon = "Trichoderma", rank = "genus",
                               check_locality = FALSE, save = TRUE, html_report = FALSE,
                               mycobank_dir = mb_dir, dir = out_dir,
                               filename = "mb_gap", verbose = FALSE)

  expect_true(file.exists(file.path(out_dir, "mb_gap.xlsx")))
  expect_gt(nrow(result), 0)
})


test_that("funga_mycobank_gap warns and caps checks at max_check", {
  skip_if_not_installed("chromote")

  mb_dir <- .mycobank_mock_dir()
  .mock_funga_mycobank_ffb(character(0))  # all 4 species-level names are "missing"

  fake_session <- structure(list(close = function(...) invisible(NULL)), class = "fake_session")
  testthat::local_mocked_bindings(
    ChromoteSession = list(new = function(...) fake_session),
    .package = "chromote"
  )
  n_calls <- 0
  testthat::local_mocked_bindings(
    .mycobank_page_details = function(session, url, taxon_name = NULL) {
      n_calls <<- n_calls + 1
      list(locality = NA_character_, substrate = NA_character_,
          etymology = NA_character_, name_type = NA_character_,
          type_specimen = NA_character_, collector = NA_character_,
          protolog = NA_character_)
    },
    .package = "fungaR"
  )

  expect_warning(
    result <- funga_mycobank_gap(taxon = "Trichoderma", rank = "genus",
                                 check_locality = TRUE, require_brazil_evidence = FALSE,
                                 max_check = 2, save = FALSE, html_report = FALSE,
                                 mycobank_dir = mb_dir, verbose = FALSE),
    "max_check"
  )
  expect_equal(n_calls, 2)
  expect_gt(nrow(result), 2)  # more candidates exist than were checked
})


test_that("funga_mycobank_gap skips the locality check gracefully without chromote", {
  mb_dir <- .mycobank_mock_dir()
  .mock_funga_mycobank_ffb(c("Trichoderma harzianum"))

  testthat::local_mocked_bindings(
    requireNamespace = function(pkg, ...) if (identical(pkg, "chromote")) FALSE else TRUE,
    .package = "base"
  )

  result <- funga_mycobank_gap(taxon = "Trichoderma", rank = "genus",
                               check_locality = TRUE, save = FALSE, html_report = FALSE,
                               mycobank_dir = mb_dir, verbose = FALSE)

  expect_false("MycoBank_Locality" %in% names(result))
  expect_gt(nrow(result), 0)
})
