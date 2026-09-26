.mycobank_records_mock_dir <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  mb <- data.frame(
    `Taxon name` = c("Trichoderma harzianum", "Trichoderma viride",
                     "Trichoderma newspeciesii", "Trichoderma notaspecies"),
    Authors = c("Rifai", "Pers.", "A.Author", "B.Author"),
    Rank = c("sp.", "sp.", "sp.", "gen."),
    `Year of effective publication` = c(1957, 1821, 2020, 1990),
    `Name status` = c("Legitimate", "Legitimate", "Legitimate", "Legitimate"),
    `MycoBank #` = c(100, 101, 102, 103),
    `Hyperlink to MB` = paste0("https://www.mycobank.org/page/Name details page/", 100:103),
    Classification = rep("Fungi, Dikarya, Ascomycota, Pezizomycotina, Sordariomycetes, Hypocreomycetidae, Hypocreales, Hypocreaceae, Trichoderma", 4),
    `Current MycoBank #` = c(100, 101, 102, 103),
    `Current name` = NA_character_,
    Synonymy = NA_character_,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  openxlsx::write.xlsx(mb, file.path(dir, "MBList.xlsx"), sheetName = "Sheet1")
  dir
}

.mock_mycobank_records_locality_session <- function(brazil_mb_numbers, env = parent.frame()) {
  fake_session <- structure(list(close = function(...) invisible(NULL)), class = "fake_session")
  testthat::local_mocked_bindings(
    ChromoteSession = list(new = function(...) fake_session),
    .package = "chromote",
    .env = env
  )
  # brazil_mb_numbers: MycoBank # values (as they appear at the end of the
  # mocked "Hyperlink to MB" URLs) that should resolve to a Brazilian locality.
  pattern <- if (length(brazil_mb_numbers) == 0) NULL else
    paste0("(", paste(brazil_mb_numbers, collapse = "|"), ")$")
  testthat::local_mocked_bindings(
    .mycobank_page_details = function(session, url, taxon_name = NULL) {
      if (!is.null(pattern) && grepl(pattern, url)) {
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


test_that("mycobank_records retrieves all matching species-level names for a genus", {
  mb_dir <- .mycobank_records_mock_dir()

  result <- mycobank_records(taxon = "Trichoderma", rank = "genus",
                                   check_locality = FALSE, save = FALSE, html_report = FALSE,
                                   mycobank_dir = mb_dir, verbose = FALSE)

  expect_setequal(result$Taxon_name, c("Trichoderma harzianum", "Trichoderma viride",
                                       "Trichoderma newspeciesii"))
})


test_that("mycobank_records excludes non-species ranks (e.g. the genus entry itself)", {
  mb_dir <- .mycobank_records_mock_dir()

  result <- mycobank_records(taxon = "Trichoderma", rank = "genus",
                                   check_locality = FALSE, save = FALSE, html_report = FALSE,
                                   mycobank_dir = mb_dir, verbose = FALSE)

  expect_false("Trichoderma" %in% result$Taxon_name)
})


test_that("mycobank_records retrieves a single exact species match", {
  mb_dir <- .mycobank_records_mock_dir()

  result <- mycobank_records(taxon = "Trichoderma harzianum", rank = "species",
                                   check_locality = FALSE, save = FALSE, html_report = FALSE,
                                   mycobank_dir = mb_dir, verbose = FALSE)

  expect_equal(nrow(result), 1)
  expect_equal(result$Taxon_name, "Trichoderma harzianum")
})


test_that("mycobank_records returns an empty data.frame (not an error) with no matches", {
  mb_dir <- .mycobank_records_mock_dir()

  result <- mycobank_records(taxon = "Nonexistentus fakus", rank = "species",
                                   check_locality = FALSE, save = FALSE, html_report = FALSE,
                                   mycobank_dir = mb_dir, verbose = FALSE)

  expect_equal(nrow(result), 0)
})


test_that("mycobank_records filters to only Brazil-evidence records by default", {
  skip_if_not_installed("chromote")

  mb_dir <- .mycobank_records_mock_dir()
  .mock_mycobank_records_locality_session(brazil_mb_numbers = "101")

  result <- mycobank_records(taxon = "Trichoderma", rank = "genus",
                                   check_locality = TRUE, save = FALSE, html_report = FALSE,
                                   mycobank_dir = mb_dir, verbose = FALSE)

  expect_equal(nrow(result), 1)
  expect_equal(result$Taxon_name, "Trichoderma viride")
  expect_true(result$MycoBank_Brazil_Evidence)
})


test_that("mycobank_records returns an empty data.frame (not an error) when no candidate has Brazil evidence", {
  skip_if_not_installed("chromote")

  mb_dir <- .mycobank_records_mock_dir()
  .mock_mycobank_records_locality_session(brazil_mb_numbers = character(0))

  result <- mycobank_records(taxon = "Trichoderma", rank = "genus",
                                   check_locality = TRUE, save = FALSE, html_report = FALSE,
                                   mycobank_dir = mb_dir, verbose = FALSE)

  expect_equal(nrow(result), 0)
})


test_that("mycobank_records returns unfiltered records when require_brazil_evidence = FALSE", {
  skip_if_not_installed("chromote")

  mb_dir <- .mycobank_records_mock_dir()
  .mock_mycobank_records_locality_session(brazil_mb_numbers = "101")

  result <- mycobank_records(taxon = "Trichoderma", rank = "genus",
                                   check_locality = TRUE, require_brazil_evidence = FALSE,
                                   save = FALSE, html_report = FALSE,
                                   mycobank_dir = mb_dir, verbose = FALSE)

  expect_equal(nrow(result), 3)
  expect_true("MycoBank_Brazil_Evidence" %in% names(result))
})


test_that("mycobank_records ignores require_brazil_evidence when check_locality = FALSE", {
  mb_dir <- .mycobank_records_mock_dir()

  result <- mycobank_records(taxon = "Trichoderma", rank = "genus",
                                   check_locality = FALSE, require_brazil_evidence = TRUE,
                                   save = FALSE, html_report = FALSE,
                                   mycobank_dir = mb_dir, verbose = FALSE)

  expect_equal(nrow(result), 3)
})


test_that("mycobank_records requires a single character taxon", {
  expect_error(mycobank_records(taxon = NULL), "single character string")
  expect_error(mycobank_records(taxon = c("A", "B")), "single character string")
})


test_that("mycobank_records warns and caps checks at max_check", {
  skip_if_not_installed("chromote")

  mb_dir <- .mycobank_records_mock_dir()

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
    result <- mycobank_records(taxon = "Trichoderma", rank = "genus",
                                     check_locality = TRUE, require_brazil_evidence = FALSE,
                                     max_check = 2, save = FALSE, html_report = FALSE,
                                     mycobank_dir = mb_dir, verbose = FALSE),
    "max_check"
  )
  expect_equal(n_calls, 2)
  expect_gt(nrow(result), 2)
})


test_that("mycobank_records skips the locality check gracefully without chromote", {
  mb_dir <- .mycobank_records_mock_dir()

  testthat::local_mocked_bindings(
    requireNamespace = function(pkg, ...) if (identical(pkg, "chromote")) FALSE else TRUE,
    .package = "base"
  )

  result <- mycobank_records(taxon = "Trichoderma", rank = "genus",
                                   check_locality = TRUE, save = FALSE, html_report = FALSE,
                                   mycobank_dir = mb_dir, verbose = FALSE)

  expect_false("MycoBank_Locality" %in% names(result))
  expect_equal(nrow(result), 3)
})


test_that("mycobank_records writes a real .xlsx file when save = TRUE", {
  mb_dir <- .mycobank_records_mock_dir()
  out_dir <- withr::local_tempdir()

  result <- mycobank_records(taxon = "Trichoderma", rank = "genus",
                                   check_locality = FALSE, save = TRUE, html_report = FALSE,
                                   mycobank_dir = mb_dir, dir = out_dir,
                                   filename = "mb_records", verbose = FALSE)

  expect_true(file.exists(file.path(out_dir, "mb_records.xlsx")))
  expect_gt(nrow(result), 0)
})


test_that("mycobank_records renders a real HTML report when html_report = TRUE", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  mb_dir <- .mycobank_records_mock_dir()
  out_dir <- withr::local_tempdir()

  result <- mycobank_records(taxon = "Trichoderma", rank = "genus",
                                   check_locality = FALSE, save = FALSE, html_report = TRUE,
                                   open_report = FALSE, mycobank_dir = mb_dir,
                                   dir = out_dir, filename = "report_test", verbose = FALSE)

  expect_true(file.exists(file.path(out_dir, "report_test.html")))
})


test_that("mycobank_records reuses a previously downloaded MycoBank list", {
  mb_dir <- .mycobank_records_mock_dir()

  testthat::local_mocked_bindings(
    download.file = function(...) stop("should not re-download when cache exists"),
    .package = "utils"
  )

  expect_no_error(
    mycobank_records(taxon = "Trichoderma", rank = "genus", check_locality = FALSE,
                           save = FALSE, html_report = FALSE, mycobank_dir = mb_dir,
                           verbose = FALSE)
  )
})
