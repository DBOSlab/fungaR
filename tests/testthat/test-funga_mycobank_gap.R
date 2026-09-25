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


test_that("funga_mycobank_gap flags MycoBank_Brazil_Evidence from each name's own locality", {
  skip_if_not_installed("chromote")

  mb_dir <- .mycobank_mock_dir()
  .mock_funga_mycobank_ffb(c("Trichoderma harzianum"))

  fake_session <- structure(list(close = function(...) invisible(NULL)), class = "fake_session")
  testthat::local_mocked_bindings(
    ChromoteSession = list(new = function(...) fake_session),
    .package = "chromote"
  )
  testthat::local_mocked_bindings(
    .mycobank_page_locality = function(session, url) {
      # Brazilian locality for the candidate whose MycoBank URL ends in
      # "101" (Trichoderma viride in the fixture); non-Brazilian otherwise.
      if (grepl("101$", url)) {
        list(locality = "Brazil, Bahia", substrate = "on dead wood")
      } else {
        list(locality = "Australia", substrate = "on living Eucalyptus")
      }
    },
    .package = "fungaR"
  )

  result <- funga_mycobank_gap(taxon = "Trichoderma", rank = "genus",
                               check_locality = TRUE, save = FALSE, html_report = FALSE,
                               mycobank_dir = mb_dir, verbose = FALSE)

  expect_true("MycoBank_Brazil_Evidence" %in% names(result))
  viride_row <- result[result$Taxon_name == "Trichoderma viride", ]
  expect_true(viride_row$MycoBank_Brazil_Evidence)
  expect_equal(viride_row$MycoBank_Locality, "Brazil, Bahia")

  newsp_row <- result[result$Taxon_name == "Trichoderma newspeciesii", ]
  expect_false(newsp_row$MycoBank_Brazil_Evidence)
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
    .mycobank_page_locality = function(session, url) {
      n_calls <<- n_calls + 1
      list(locality = NA_character_, substrate = NA_character_)
    },
    .package = "fungaR"
  )

  expect_warning(
    result <- funga_mycobank_gap(taxon = "Trichoderma", rank = "genus",
                                 check_locality = TRUE, max_check = 2,
                                 save = FALSE, html_report = FALSE,
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
