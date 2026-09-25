test_that(".filter_occur_df filters by family, genus, and species", {
  df <- data.frame(
    family = c("Hypocreaceae", "Hypocreaceae", "Boletaceae"),
    genus = c("Trichoderma", "Xylaria", "Boletus"),
    taxonName = c("Trichoderma harzianum", "Xylaria hypoxylon", "Boletus edulis"),
    stringsAsFactors = FALSE
  )

  fam_result <- .filter_occur_df(df, taxon = "Hypocreaceae", state = NULL, verbose = FALSE)
  expect_setequal(fam_result$taxonName, c("Trichoderma harzianum", "Xylaria hypoxylon"))

  gen_result <- .filter_occur_df(df, taxon = "Xylaria", state = NULL, verbose = FALSE)
  expect_equal(gen_result$taxonName, "Xylaria hypoxylon")

  sp_result <- .filter_occur_df(df, taxon = "Trichoderma harzianum", state = NULL, verbose = FALSE)
  expect_equal(sp_result$taxonName, "Trichoderma harzianum")
})


test_that(".filter_occur_df filters by state when stateProvince is present", {
  df <- data.frame(
    family = c("Hypocreaceae", "Hypocreaceae"),
    genus = c("Trichoderma", "Xylaria"),
    taxonName = c("Trichoderma harzianum", "Xylaria hypoxylon"),
    stateProvince = c("Bahia", "Minas Gerais"),
    stringsAsFactors = FALSE
  )
  result <- .filter_occur_df(df, taxon = NULL, state = "Bahia", verbose = FALSE)
  expect_equal(result$taxonName, "Trichoderma harzianum")
})


test_that(".save_csv writes a CSV file to disk", {
  tmp_dir <- tempfile("funga_save_csv_")
  df <- data.frame(x = 1:3, y = letters[1:3])

  .save_csv(df, verbose = FALSE, filename = "test_file", dir = tmp_dir)

  out <- file.path(tmp_dir, "test_file.csv")
  expect_true(file.exists(out))
  written <- read.csv(out)
  expect_equal(nrow(written), 3)
  expect_equal(ncol(written), 2)

  unlink(tmp_dir, recursive = TRUE)
})


test_that(".save_log writes a summary log file", {
  tmp_dir <- tempfile("funga_save_log_")
  dir.create(tmp_dir)
  df <- data.frame(
    family = c("Hypocreaceae", "Hypocreaceae"),
    genus = c("Trichoderma", "Xylaria"),
    country = c("Brazil", "Brazil"),
    stateProvince = c("Bahia", "Bahia"),
    stringsAsFactors = FALSE
  )

  .save_log(df, filename = "test_file", dir = tmp_dir)

  log_path <- file.path(tmp_dir, "log.txt")
  expect_true(file.exists(log_path))
  log_contents <- readLines(log_path)
  expect_true(any(grepl("Total records: 2", log_contents)))
  expect_true(any(grepl("Records per family:", log_contents)))

  unlink(tmp_dir, recursive = TRUE)
})


test_that(".funga_get_taxon extracts the taxon.txt table from a dwca list", {
  dwca <- list(
    dwca_ffb_v1 = list(data = list(taxon.txt = data.frame(id = "1", taxonName = "Trichoderma harzianum")))
  )
  result <- .funga_get_taxon(dwca)
  expect_equal(result$taxonName, "Trichoderma harzianum")

  expect_error(.funga_get_taxon(list()), "non-empty named list")
  expect_error(.funga_get_taxon(list(x = list(data = list()))), "No 'taxon.txt' table found")
})


test_that(".funga_get_col safely accesses a column or returns NA vector", {
  df <- data.frame(a = 1:3)
  expect_equal(.funga_get_col(df, "a"), 1:3)
  expect_equal(.funga_get_col(df, "missing"), rep(NA_character_, 3))
})


test_that(".funga_names_standardize trims and collapses whitespace", {
  expect_equal(.funga_names_standardize("  Trichoderma   harzianum  "), "Trichoderma harzianum")
  expect_equal(.funga_names_standardize(c("A  B", " C ")), c("A B", "C"))
})


test_that(".funga_splist_classify parses genus, epithet, infra rank, and author", {
  result <- .funga_splist_classify(c(
    "Trichoderma harzianum",
    "Ganoderma lucidum subsp. esculentum Mill.",
    "Xylaria hypoxylon L."
  ))

  expect_equal(result$genus, c("Trichoderma", "Ganoderma", "Xylaria"))
  expect_equal(result$epithet, c("harzianum", "lucidum", "hypoxylon"))
  expect_equal(result$infra_rank[2], "subsp.")
  expect_equal(result$infra_epithet[2], "esculentum")
  expect_equal(result$author[2], "Mill.")
  expect_equal(result$author[3], "L.")
  expect_true(is.na(result$infra_rank[1]))
})


test_that(".funga_splist_classify handles NA and empty input gracefully", {
  result <- .funga_splist_classify(c(NA_character_, ""))
  expect_true(all(is.na(result$genus)))
  expect_true(all(is.na(result$epithet)))
})


test_that(".funga_build_genus_index builds a genus-to-row-index lookup", {
  taxon_df <- data.frame(genus = c("Trichoderma", "Trichoderma", "Xylaria"), stringsAsFactors = FALSE)
  idx <- .funga_build_genus_index(taxon_df)
  expect_equal(sort(idx[["Trichoderma"]]), c(1L, 2L))
  expect_equal(idx[["Xylaria"]], 3L)
})


test_that(".funga_get_threshold computes integer or fractional Levenshtein thresholds", {
  expect_equal(.funga_get_threshold(2, 10), 2L)
  expect_equal(.funga_get_threshold(0.2, 10), 2L)
  expect_equal(.funga_get_threshold(0.01, 10), 1L)  # minimum of 1
})


test_that(".funga_resolve_accepted resolves synonyms to their accepted name", {
  taxon_df <- data.frame(
    id = c("1", "2"),
    taxonomicStatus = c("SINONIMO", "NOME_ACEITO"),
    acceptedNameUsageID = c("2", NA_character_),
    taxonName = c("Trichoderma synonym", "Trichoderma harzianum"),
    stringsAsFactors = FALSE
  )
  id_lookup <- list("2" = 2L)

  syn_result <- .funga_resolve_accepted(1L, taxon_df, id_lookup)
  expect_equal(syn_result$id, "2")
  expect_equal(syn_result$name, "Trichoderma harzianum")

  acc_result <- .funga_resolve_accepted(2L, taxon_df, id_lookup)
  expect_equal(acc_result$id, "2")
  expect_equal(acc_result$name, "Trichoderma harzianum")
})


test_that(".funga_resolve_accepted warns and returns NA when the accepted ID is missing", {
  taxon_df <- data.frame(
    id = "1",
    taxonomicStatus = "SINONIMO",
    acceptedNameUsageID = "999",
    taxonName = "Trichoderma synonym",
    stringsAsFactors = FALSE
  )
  expect_warning(result <- .funga_resolve_accepted(1L, taxon_df, list()),
                 "not found in id_lookup")
  expect_true(is.na(result$id))
})


test_that(".funga_na_row builds a single NA-filled row, optionally with Correct.Spelling", {
  row <- .funga_na_row("Unknown species")
  expect_equal(nrow(row), 1)
  expect_equal(row$Search, "Unknown species")
  expect_true(is.na(row$FFB.taxon.ID))
  expect_false("Correct.Spelling" %in% names(row))

  row2 <- .funga_na_row("Unknown species", include_correct = TRUE)
  expect_true("Correct.Spelling" %in% names(row2))
  expect_true(is.na(row2$Correct.Spelling))
})


test_that(".funga_build_rows builds result rows resolving accepted names", {
  taxon_df <- data.frame(
    id = c("1", "2"),
    taxonRank = c("ESPECIE", "ESPECIE"),
    taxonomicStatus = c("NOME_ACEITO", "NOME_ACEITO"),
    taxonName = c("Trichoderma harzianum", "Trichoderma viride"),
    scientificNameAuthorship = c("Mart.", "L."),
    family = c("Hypocreaceae", "Hypocreaceae"),
    order = c("Hypocreales", "Hypocreales"),
    stringsAsFactors = FALSE
  )
  id_lookup <- list()

  result <- .funga_build_rows("Trichoderma harzianum", rows = 1L, dists = 0L, taxon_df, id_lookup)
  expect_equal(result$Search, "Trichoderma harzianum")
  expect_equal(result$FFB.taxon.ID, "1")
  expect_equal(result$family, "Hypocreaceae")
  expect_equal(result$Accepted.taxon.ID, "1")
  expect_equal(result$Accepted.taxon.Name, "Trichoderma harzianum")
})


test_that(".funga_find_dups flags duplicated Accepted.taxon.Name entries", {
  result <- data.frame(
    Accepted.taxon.Name = c("Trichoderma harzianum", "Xylaria hypoxylon", "Trichoderma harzianum", NA_character_)
  )
  dups <- .funga_find_dups(result)
  expect_equal(dups[1], 3L)
  expect_true(is.na(dups[2]))
  expect_equal(dups[3], 1L)
  expect_true(is.na(dups[4]))
})
