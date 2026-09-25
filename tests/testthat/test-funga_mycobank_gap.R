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
                               check_occurrence = FALSE, save = FALSE,
                               mycobank_dir = mb_dir, verbose = FALSE)

  expect_setequal(result$Taxon_name, c("Trichoderma viride", "Trichoderma newspeciesii"))
  expect_true(all(!result$In_FFB))
})


test_that("funga_mycobank_gap excludes MycoBank synonyms whose accepted name is already in FFB", {
  mb_dir <- .mycobank_mock_dir()
  # "Trichoderma oldsynonym"'s Current name is "Trichoderma harzianum", already in FFB
  .mock_funga_mycobank_ffb(c("Trichoderma harzianum"))

  result <- funga_mycobank_gap(taxon = "Trichoderma", rank = "genus",
                               check_occurrence = FALSE, save = FALSE,
                               mycobank_dir = mb_dir, verbose = FALSE)

  expect_false("Trichoderma oldsynonym" %in% result$Taxon_name)
})


test_that("funga_mycobank_gap excludes non-species ranks (e.g. the genus entry itself)", {
  mb_dir <- .mycobank_mock_dir()
  .mock_funga_mycobank_ffb(character(0))

  result <- funga_mycobank_gap(taxon = "Trichoderma", rank = "genus",
                               check_occurrence = FALSE, save = FALSE,
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
                               check_occurrence = FALSE, save = FALSE,
                               mycobank_dir = mb_dir, verbose = FALSE)

  expect_setequal(result$Taxon_name, c("Trichoderma harzianum", "Trichoderma viride",
                                       "Trichoderma newspeciesii"))
})


test_that("funga_mycobank_gap requires a single character taxon", {
  expect_error(funga_mycobank_gap(taxon = NULL), "single character string")
  expect_error(funga_mycobank_gap(taxon = c("A", "B")), "single character string")
})


test_that("funga_mycobank_gap reuses a previously downloaded MycoBank list", {
  mb_dir <- .mycobank_mock_dir()
  .mock_funga_mycobank_ffb(character(0))

  testthat::local_mocked_bindings(
    download.file = function(...) stop("should not re-download when cache exists"),
    .package = "utils"
  )

  expect_no_error(
    funga_mycobank_gap(taxon = "Trichoderma", rank = "genus", check_occurrence = FALSE,
                       save = FALSE, mycobank_dir = mb_dir, verbose = FALSE)
  )
})
