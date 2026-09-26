# Auxiliary functions to support main functions
# Author: Domingos Cardoso

# The 26 Brazilian states plus the Federal District (name = full name,
# value = 2-letter abbreviation). Shared by .arg_check_state() (state-argument
# validation) and .location_mentions_brazil() (checking whether an external
# repository's free-text locality names a Brazilian state without necessarily
# naming the country itself, e.g. Index Fungorum's LOCATION field).
.br_states <- c("Acre" = "AC", "Alagoas" = "AL", "Amap\u00e1" = "AP", "Amazonas" = "AM",
                "Bahia" = "BA", "Cear\u00e1" = "CE", "Distrito Federal" = "DF",
                "Esp\u00edrito Santo" = "ES", "Goi\u00e1s" = "GO", "Maranh\u00e3o" = "MA",
                "Mato Grosso" = "MT", "Mato Grosso do Sul" = "MS", "Minas Gerais" = "MG",
                "Par\u00e1" = "PA", "Para\u00edba" = "PB", "Paran\u00e1" = "PR", "Pernambuco" = "PE",
                "Piau\u00ed" = "PI", "Rio de Janeiro" = "RJ", "Rio Grande do Norte" = "RN",
                "Rio Grande do Sul" = "RS", "Rond\u00f4nia" = "RO", "Roraima" = "RR",
                "Santa Catarina" = "SC", "S\u00e3o Paulo" = "SP", "Sergipe" = "SE",
                "Tocantins" = "TO")


#_______________________________________________________________________________
# Function to filter occurrence data ####
.filter_occur_df <- function(occur_df, taxon, state, verbose) {

  temp_occur_df <- data.frame(matrix(ncol = length(names(occur_df)), nrow = 0))
  colnames(temp_occur_df) <- names(occur_df)

  # Filter by taxon only

  if (!is.null(taxon)) {
    if (verbose) {
      message("\nFiltering taxon names... ")
    }

    .check_taxon_match(occur_df, taxon, verbose)

    tf_fam <- grepl("aceae$", taxon)
    if (any(tf_fam)) {
      taxon_fam <- taxon[tf_fam]
      tf <- occur_df$family %in% taxon_fam
      if (any(tf)) {
        occur_df_fam <- occur_df[tf, ]
        temp_occur_df <- occur_df_fam
      }
    }

    tf_gen <- grepl("^[^ ]+$", taxon) & !grepl("aceae$", taxon)
    if (any(tf_gen)) {
      taxon_gen <- taxon[tf_gen]
      tf <- occur_df$genus %in% taxon_gen
      if (any(tf)) {
        occur_df_gen <- occur_df[tf, ]
        temp_occur_df <- rbind(temp_occur_df, occur_df_gen)
      }
    }

    tf_spp <- grepl("\\s", taxon)
    if (any(tf_spp)) {
      taxon_spp <- taxon[tf_spp]
      tf <- occur_df$taxonName %in% taxon_spp
      if (any(tf)) {
        occur_df_spp <- occur_df[tf, ]
        temp_occur_df <- rbind(temp_occur_df, occur_df_spp)
      }
    }

    if (nrow(temp_occur_df) != 0){
      occur_df <- temp_occur_df
    }

  }

  # Filter by state only ####

  if (!is.null(state)) {
    if (verbose) {
      message("\nFiltering states... ")
    }

    .check_state_match(occur_df, state, verbose)

    tf <- occur_df$stateProvince %in% state
    if (any(tf)) {
      occur_df <- occur_df[tf, ]
    }
  }

  return(occur_df)
}


#_______________________________________________________________________________
# Function to save csv files ####
.save_csv <- function(df,
                      verbose = TRUE,
                      filename = NULL,
                      dir = dir) {

  # Save the data frame if param save is TRUE
  # Create a new directory to save the results with current date
  # If there is no directory... make one!

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  filename <- paste0(filename, ".csv")
  # Create and save the spreadsheet in .csv format
  if (verbose) {
    message(paste0("Writing spreadsheet '",
                   filename, "' within '",
                   dir, "' folder on disk."))
  }
  utils::write.csv(df, file = paste0(dir, "/", filename), row.names = FALSE)
}


#_______________________________________________________________________________
# Function to save xlsx spreadsheets (used by the MycoBank/distribution gap
# checkers, which return richer, multi-source tables better suited to a
# proper spreadsheet than a plain CSV) ####
.save_xlsx <- function(df,
                       verbose = TRUE,
                       filename = NULL,
                       dir = dir) {

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  filename <- paste0(filename, ".xlsx")
  if (verbose) {
    message(paste0("Writing spreadsheet '",
                   filename, "' within '",
                   dir, "' folder on disk."))
  }
  openxlsx::write.xlsx(df, file = paste0(dir, "/", filename))
}


#_______________________________________________________________________________
# Query GBIF's public occurrence API for Brazil-only evidence of a given
# scientific name. Used by mycobank_gap() and distribution_gap()
# as the primary (reliable, no API key required) occurrence-evidence source.
# Returns a list(n_records, states) and never errors -- any network/parsing
# failure is caught and reported as NA so a single bad lookup never aborts a
# batch of many taxa. ####
.gbif_brazil_occurrence <- function(name) {
  url <- paste0("https://api.gbif.org/v1/occurrence/search?scientificName=",
               utils::URLencode(name, reserved = TRUE),
               "&country=BR&limit=0&facet=STATE_PROVINCE&facetLimit=30")

  res <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)

  empty_counts <- data.frame(state = character(0), count = integer(0))

  if (is.null(res) || is.null(res$count)) {
    return(list(n_records = NA_integer_, states = NA_character_, state_counts = empty_counts))
  }

  states <- NA_character_
  state_counts <- empty_counts
  if (is.data.frame(res$facets) && nrow(res$facets) > 0) {
    counts_df <- res$facets$counts[[1]]
    if (is.data.frame(counts_df) && nrow(counts_df) > 0) {
      states <- paste(counts_df$name, collapse = "; ")
      state_counts <- data.frame(state = counts_df$name, count = as.integer(counts_df$count),
                                 stringsAsFactors = FALSE)
    }
  }

  list(n_records = as.integer(res$count), states = states, state_counts = state_counts)
}


#_______________________________________________________________________________
# Fetch individual (record-level, not aggregated) GBIF occurrence records for a
# given scientific name within one Brazilian state, each with a direct link to
# the record's own GBIF occurrence page. Used by distribution_gap() to
# list the actual specimen/observation records backing a new-state-record
# candidate, not just the state-level count. Never errors - any failure
# returns a zero-row data frame with the expected columns. ####
.gbif_brazil_state_records <- function(name, state, limit = 50) {
  wanted <- c("key", "scientificName", "stateProvince", "municipality", "locality",
             "recordedBy", "recordNumber", "eventDate", "institutionCode",
             "collectionCode", "catalogNumber", "decimalLatitude", "decimalLongitude")
  empty <- as.data.frame(stats::setNames(rep(list(character(0)), length(wanted)), wanted),
                         stringsAsFactors = FALSE)

  url <- paste0("https://api.gbif.org/v1/occurrence/search?scientificName=",
               utils::URLencode(name, reserved = TRUE),
               "&country=BR&stateProvince=", utils::URLencode(state, reserved = TRUE),
               "&limit=", limit)

  res <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)

  if (is.null(res) || is.null(res$results) || !is.data.frame(res$results) ||
      nrow(res$results) == 0) {
    return(empty)
  }

  df <- res$results
  for (col in setdiff(wanted, names(df))) df[[col]] <- NA
  df <- df[, wanted, drop = FALSE]
  df$GBIF_URL <- paste0("https://www.gbif.org/occurrence/", df$key)

  df
}


#_______________________________________________________________________________
# Query speciesLink's public web service for Brazil-only evidence of a given
# scientific name. speciesLink's 'api.' endpoint has known reliability issues
# (an SSL certificate that does not match its own hostname, as of this
# writing) so this is strictly best-effort: any failure is caught and
# reported as NA rather than raised, and callers should treat GBIF as the
# dependable primary source. ####
.splink_brazil_occurrence <- function(name) {
  url <- paste0("https://api.splink.org.br/index?operation=RECORDS&species=",
               utils::URLencode(name, reserved = TRUE),
               "&country=Brasil&Scope=microrganisms&MaxRecords=200")

  res <- tryCatch(suppressWarnings(jsonlite::fromJSON(url)),
                  error = function(e) NULL)

  if (is.null(res) || is.null(res$features) || length(res$features) == 0) {
    return(list(n_records = NA_integer_, states = NA_character_))
  }

  recs <- res$features
  state_col <- intersect(c("stateprovince", "stateProvince"), names(recs))
  states <- if (length(state_col) > 0) {
    paste(sort(unique(stats::na.omit(recs[[state_col[1]]]))), collapse = "; ")
  } else {
    NA_character_
  }

  list(n_records = nrow(recs), states = if (nzchar(states)) states else NA_character_)
}


#_______________________________________________________________________________
# Visit one MycoBank name page (a JavaScript single-page app with no static
# HTML fallback - a plain HTTP GET only returns an empty Angular shell) via an
# already-open chromote session, and extract the type specimen's locality,
# substrate/host, and several other fields only available on the individual
# page (not in MycoBank's bulk export): etymology, name type (e.g. Basionym/
# Combination), the type specimen voucher itself, the collector, and the
# original-publication (protolog) citation. Used by mycobank_gap() and
# mycobank_records() to check whether MycoBank itself already
# associates a candidate name with a Brazilian locality, and to enrich the
# returned spreadsheet beyond what the bulk export alone provides. Never
# errors - any navigation or parsing failure is caught and every field
# reported as NA, matching the package's defensive pattern for other
# external lookups. ####
.mycobank_page_details <- function(session, url, taxon_name = NULL, max_wait = 20,
                                   retries = 1) {
  # A data-driven marker guaranteed to appear in document.body.innerText only
  # once the record's own async data has actually rendered: the requested
  # taxon name itself when known (most precise - passed by the caller, which
  # already knows which name it expects), else the "MycoBank #" data label
  # that is present on every rendered name page regardless of whether that
  # particular record has locality data. Polling document.title for a change
  # away from the generic app-shell title was tried first but proved
  # unreliable: title and the specimen/locality section can be populated by
  # separate async fetches, so title sometimes updates before the content we
  # actually need to scrape has rendered - causing intermittent false "no
  # locality" results even for names MycoBank does report a locality for.
  marker <- if (!is.null(taxon_name) && nzchar(taxon_name)) taxon_name else "MycoBank #"

  one_attempt <- function() {
    session$Page$navigate(url)
    session$Page$loadEventFired(wait_ = TRUE, timeout_ = 20)

    deadline <- Sys.time() + max_wait
    val <- NULL
    found <- FALSE
    repeat {
      val <- session$Runtime$evaluate("document.body.innerText")$result$value
      if (!is.null(val) && !is.na(val) && grepl(marker, val, fixed = TRUE)) {
        found <- TRUE
        break
      }
      if (Sys.time() >= deadline) break
      Sys.sleep(0.5)
    }
    list(val = val, found = found)
  }

  txt <- tryCatch({
    val <- NULL
    for (attempt in seq_len(retries + 1)) {
      res <- one_attempt()
      val <- res$val
      # A network/render hiccup that never produces the marker within
      # max_wait is retried once (a fresh navigation, not just re-polling the
      # same failed load) rather than immediately accepted as "no locality" -
      # this scrape is occasionally flaky under real network conditions even
      # for records confirmed (by hand) to have the data we're looking for.
      if (res$found || attempt > retries) break
    }
    val
  }, error = function(e) NA_character_)

  empty <- list(locality = NA_character_, substrate = NA_character_,
               etymology = NA_character_, name_type = NA_character_,
               type_specimen = NA_character_, collector = NA_character_,
               protolog = NA_character_)

  if (is.na(txt) || !nzchar(txt)) {
    return(empty)
  }

  lines <- strsplit(txt, "\n")[[1]]
  # Matches the first occurrence of `label` - the quick-summary "Type
  # information" box near the top of the page always lists each of these as
  # its own line followed immediately by the value on the next line, which is
  # simpler and more reliable than the later "Specimen details" table further
  # down the page (a tab-separated table repeating some of the same fields).
  next_line_after <- function(label, fixed = TRUE) {
    idx <- if (fixed) which(lines == label) else grep(label, lines)
    if (length(idx) == 0 || idx[1] >= length(lines)) return(NA_character_)
    val <- trimws(lines[idx[1] + 1])
    if (!nzchar(val)) NA_character_ else val
  }

  list(locality = next_line_after("Location details"),
      substrate = next_line_after("Substrate details"),
      etymology = next_line_after("Etymology"),
      name_type = next_line_after("Name type"),
      # The parenthetical status (holotype/isotype/lectotype/...) varies per
      # record, so match the fixed label prefix rather than an exact line.
      type_specimen = next_line_after("^Type specimen or ex type", fixed = FALSE),
      collector = next_line_after("Collection details"),
      protolog = next_line_after("Protolog"))
}


#_______________________________________________________________________________
# Whether a free-text locality string mentions Brazil - either the country
# name itself, or (Index Fungorum's LOCATION field very often gives a
# Brazilian state/province name directly, e.g. "Pernambuco", without ever
# naming the country) any of the 26 Brazilian states plus the Federal
# District. Diacritics are stripped on both sides so e.g. "Sao Paulo" or
# "Ceara" (without accents) still match. ####
.location_mentions_brazil <- function(location) {
  if (is.null(location) || is.na(location) || !nzchar(location)) return(FALSE)

  loc_ascii <- stringi::stri_trans_general(location, "Latin-ASCII")
  if (grepl("brazil|brasil", loc_ascii, ignore.case = TRUE)) return(TRUE)

  states_ascii <- stringi::stri_trans_general(names(.br_states), "Latin-ASCII")
  any(vapply(states_ascii, function(st) grepl(st, loc_ascii, ignore.case = TRUE),
            logical(1)))
}


#_______________________________________________________________________________
# Query Index Fungorum's own web service (a plain HTTP GET/XML API, not SOAP -
# https://www.indexfungorum.org/ixfwebservice/fungus.asmx) for every name
# matching a species, genus, or order, and return it as a data.frame. Unlike
# MycoBank, this needs no bulk-export download or per-record page visit (and
# no headless browser): a single request returns every matching name,
# including - for many records - the type specimen's own reported LOCATION
# and HOST, which is what spfungorum_gap()/spfungorum_records() use as
# their Brazil-evidence signal. Never errors - a request failure returns an
# empty (but correctly shaped) data.frame, matching the package's defensive
# pattern for other external lookups. ####
.indexfungorum_name_search <- function(taxon, rank = c("species", "genus"),
                                       max_number = 1000) {
  rank <- match.arg(rank)

  empty <- data.frame(Taxon_name = character(0), Authors = character(0),
                      Year = character(0), Rank = character(0),
                      Name_status = character(0), Current_name = character(0),
                      Location = character(0), Host = character(0),
                      Fungorum_Number = character(0), stringsAsFactors = FALSE)

  doc <- tryCatch({
    url <- sprintf(
      "https://www.indexfungorum.org/ixfwebservice/fungus.asmx/NameSearch?SearchText=%s&AnywhereInText=false&MaxNumber=%d",
      utils::URLencode(taxon, reserved = TRUE), max_number)
    xml2::read_xml(url)
  }, error = function(e) NULL)

  if (is.null(doc)) return(empty)

  nodes <- xml2::xml_find_all(doc, ".//IndexFungorum")
  if (length(nodes) == 0) return(empty)

  get_field <- function(node, field) {
    val <- xml2::xml_text(xml2::xml_find_first(node, field))
    if (is.na(val) || !nzchar(val)) return(NA_character_)
    # Index Fungorum's XML stores HTML markup (e.g. "<i>...</i>" around a
    # species epithet within HOST) as literal escaped text, not real child
    # elements, so xml_text() alone does not strip it - remove it here.
    val <- trimws(gsub("<[^>]+>", "", val))
    if (!nzchar(val)) NA_character_ else val
  }

  result <- do.call(rbind, lapply(nodes, function(node) {
    data.frame(
      Taxon_name = get_field(node, "NAME_x0020_OF_x0020_FUNGUS"),
      Authors = get_field(node, "AUTHORS"),
      Year = get_field(node, "YEAR_x0020_OF_x0020_PUBLICATION"),
      Rank = get_field(node, "INFRASPECIFIC_x0020_RANK"),
      Name_status = get_field(node, "NAME_x0020_STATUS"),
      Current_name = get_field(node, "CURRENT_x0020_NAME"),
      Location = get_field(node, "LOCATION"),
      Host = get_field(node, "HOST"),
      Fungorum_Number = get_field(node, "RECORD_x0020_NUMBER"),
      stringsAsFactors = FALSE
    )
  }))
  rownames(result) <- NULL

  # Keep species-level names only - for a genus-rank search this also drops
  # the genus's own entry (Rank == "gen.") that "from the start" matching
  # otherwise includes alongside its species.
  result <- result[result$Rank %in% "sp." & !is.na(result$Taxon_name), ]
  rownames(result) <- NULL
  result
}


#_______________________________________________________________________________
# Render one of the funga_*_gap() HTML reports (inst/rmd/<template>) from a
# pre-computed data list, mirroring the jabotR HTML-report pattern: KPI boxes
# plus filterable/downloadable DT tables. Used by mycobank_gap() and
# distribution_gap(). Degrades gracefully (message + skip) if the
# reporting packages or the template are unavailable, so a missing Suggests
# dependency never breaks the underlying analysis. ####
.funga_render_report <- function(template, data_list, taxon, dir, filename,
                                 verbose, open_report) {

  if (!requireNamespace("rmarkdown", quietly = TRUE) ||
      !requireNamespace("DT", quietly = TRUE) ||
      !requireNamespace("htmltools", quietly = TRUE)) {
    if (verbose) {
      message("  Packages 'rmarkdown', 'DT', and 'htmltools' are required for the HTML ",
             "report; skipping it (the spreadsheet was still saved). Install them with ",
             "install.packages(c('rmarkdown', 'DT', 'htmltools')).")
    }
    return(invisible(FALSE))
  }

  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  rmd_template <- system.file("rmd", template, package = "fungaR")
  if (!nzchar(rmd_template)) {
    if (verbose) message("  HTML report template not found; skipping.")
    return(invisible(FALSE))
  }

  tmp_rds <- tempfile(fileext = ".rds")
  saveRDS(data_list, tmp_rds)
  on.exit(unlink(tmp_rds), add = TRUE)

  html_out <- file.path(normalizePath(dir), paste0(filename, ".html"))

  if (verbose) message("Rendering HTML report...")
  render_ok <- tryCatch({
    rmarkdown::render(
      input = rmd_template,
      output_file = html_out,
      params = list(data_path = tmp_rds, taxon = taxon),
      envir = new.env(parent = globalenv()),
      quiet = !verbose
    )
    TRUE
  }, error = function(e) {
    if (verbose) message("  HTML report rendering failed: ", conditionMessage(e))
    FALSE
  })

  if (render_ok) {
    if (verbose) message("Report saved: ", html_out)
    if (open_report && interactive()) utils::browseURL(html_out)
  }

  invisible(render_ok)
}


#_______________________________________________________________________________
# Function to save log.txt file ####
.save_log <- function(df,
                      filename = NULL,
                      dir = dir) {

  log_line <- sprintf("[%s] Downloaded: %s | Records saved to: %s/%s.csv\n",
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                      nrow(df),
                      dir,
                      filename)

  # Add summary statistics
  count_total <- nrow(df)
  by_family <- utils::capture.output(print(table(df$family)))
  by_genus <- utils::capture.output(print(table(df$genus)))
  by_country <- utils::capture.output(print(table(df$country)))
  by_state <- utils::capture.output(print(table(df$stateProvince)))

  stats_summary <- c(
    sprintf("Total records: %d", count_total),
    "\nRecords per family:", by_family,
    "\nRecords per genus:", by_genus,
    "\nRecords per country:", by_country,
    "\nRecords per stateProvince:", by_state,
    "--------------------------------------------------\n"
  )

  write(c(log_line, stats_summary), file = file.path(dir, "log.txt"), append = TRUE)
}


#_______________________________________________________________________________
# Extract taxon data.frame from the first (most recent) version in a dwca object ####
.funga_get_taxon <- function(dwca) {
  if (!is.list(dwca) || length(dwca) == 0L) {
    stop("'dwca' must be a non-empty named list returned by funga_parse().",
         call. = FALSE)
  }
  taxon_df <- dwca[[1L]][["data"]][["taxon.txt"]]
  if (is.null(taxon_df)) {
    stop("No 'taxon.txt' table found in the dwca object. Run funga_parse() first.",
         call. = FALSE)
  }
  taxon_df
}


#_______________________________________________________________________________
# Safely access a column from a data.frame, returning NA vector if absent ####
.funga_get_col <- function(df, col) {
  if (col %in% colnames(df)) df[[col]] else rep(NA_character_, nrow(df))
}


#_______________________________________________________________________________
# Trim whitespace and collapse internal spaces in a name vector ####
.funga_names_standardize <- function(splist) {
  gsub("\\s+", " ", trimws(splist))
}


#_______________________________________________________________________________
# Parse a standardized name vector into genus / epithet / infra components. ####
# Returns a data.frame with columns: original, genus, epithet,
# infra_rank, infra_epithet, author.
.funga_splist_classify <- function(splist_std) {
  infra_markers <- c("subsp.", "var.", "f.", "fo.", "subvar.", "subf.", "forma")

  rows <- lapply(splist_std, function(name) {
    if (is.na(name) || !nzchar(trimws(name))) {
      return(list(original = NA_character_, genus = NA_character_,
                  epithet = NA_character_, infra_rank = NA_character_,
                  infra_epithet = NA_character_, author = NA_character_))
    }
    parts <- strsplit(name, " ")[[1L]]
    n <- length(parts)
    genus  <- if (n >= 1L) parts[1L] else NA_character_
    epithet <- if (n >= 2L) parts[2L] else NA_character_
    infra_rank <- NA_character_
    infra_epithet <- NA_character_
    author <- NA_character_

    if (n > 2L) {
      mk_rel <- which(parts[3L:n] %in% infra_markers)
      if (length(mk_rel) > 0L) {
        mk <- mk_rel[1L] + 2L     # re-index into full parts
        infra_rank <- parts[mk]
        infra_epithet <- if (mk < n) parts[mk + 1L] else NA_character_
        if (mk + 1L < n) author <- paste(parts[(mk + 2L):n], collapse = " ")
      } else {
        author <- paste(parts[3L:n], collapse = " ")
      }
    }

    list(original = name, genus = genus, epithet = epithet,
         infra_rank = infra_rank, infra_epithet = infra_epithet, author = author)
  })

  as.data.frame(
    do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)),
    stringsAsFactors = FALSE
  )
}


#_______________________________________________________________________________
# Build a genus -> row-index lookup list for fast genus-restricted searching ####
.funga_build_genus_index <- function(taxon_df) {
  tapply(seq_len(nrow(taxon_df)), taxon_df$genus, identity, simplify = FALSE)
}


#_______________________________________________________________________________
# Compute Levenshtein threshold: integer if max_distance >= 1, ####
# else fraction of name length (minimum 1)
.funga_get_threshold <- function(max_distance, name_len) {
  if (max_distance >= 1) as.integer(max_distance)
  else max(1L, floor(max_distance * name_len))
}


#_______________________________________________________________________________
# Search for a single classified species against the FFB taxon table. ####
# Returns list(rows, exact, distances) or NULL when nothing is within threshold.
# Set return_all = TRUE to get every match; keep_closest = TRUE returns only
# the best distance(s).
.funga_search_ind <- function(sc, taxon_df, genus_index,
                              max_distance, genus_fuzzy,
                              return_all = FALSE, keep_closest = TRUE) {
  if (is.na(sc$epithet)) return(NULL)

  search_name <- paste(sc$genus, sc$epithet)
  if (!is.na(sc$infra_epithet) && nzchar(sc$infra_epithet)) {
    search_name <- paste(search_name, sc$infra_epithet)
  }

  # --- Exact match (fastest path) ---
  exact_pos <- which(taxon_df$taxonName == search_name)
  if (length(exact_pos) > 0L) {
    return(list(rows = exact_pos, exact = TRUE,
                distances = rep(0L, length(exact_pos))))
  }

  # --- Fuzzy match ---
  threshold <- .funga_get_threshold(max_distance, nchar(search_name))

  if (genus_fuzzy) {
    cand_idx <- which(!is.na(taxon_df$taxonName))
    dists <- utils::adist(search_name, taxon_df$taxonName[cand_idx])[1L, ]
    within <- which(dists <= threshold)
    if (length(within) == 0L) return(NULL)
    rows <- cand_idx[within]
    d    <- dists[within]
  } else {
    genus_rows <- genus_index[[sc$genus]]
    if (is.null(genus_rows)) return(NULL)

    ep_search <- if (!is.na(sc$infra_epithet) && nzchar(sc$infra_epithet)) {
      paste(sc$epithet, sc$infra_epithet)
    } else sc$epithet

    cand_ep <- trimws(ifelse(
      !is.na(taxon_df$infraspecificEpithet[genus_rows]),
      paste(taxon_df$specificEpithet[genus_rows],
            taxon_df$infraspecificEpithet[genus_rows]),
      taxon_df$specificEpithet[genus_rows]
    ))
    ep_thresh <- .funga_get_threshold(max_distance, nchar(ep_search))
    dists <- utils::adist(ep_search, cand_ep)[1L, ]
    within <- which(dists <= ep_thresh)
    if (length(within) == 0L) return(NULL)
    rows <- genus_rows[within]
    d <- dists[within]
  }

  if (!return_all || keep_closest) {
    min_d <- min(d)
    keep <- which(d == min_d)
    rows <- rows[keep]
    d <- d[keep]
  }

  list(rows = rows, exact = FALSE, distances = d)
}


#_______________________________________________________________________________
# Resolve a single taxon row index to its accepted name and ID. ####
# id_lookup is a list mapping FFB id strings to row positions in taxon_df.
.funga_resolve_accepted <- function(row_idx, taxon_df, id_lookup) {
  status <- taxon_df$taxonomicStatus[row_idx]

  if (!is.na(status) && status == "SINONIMO" &&
      !is.na(taxon_df$acceptedNameUsageID[row_idx])) {
    acc_id <- as.character(taxon_df$acceptedNameUsageID[row_idx])

    # Verificar se o acc_id existe no id_lookup
    acc_pos <- id_lookup[[acc_id]]

    if (!is.null(acc_pos) && length(acc_pos) > 0) {
      # Verificar se acc_pos[1] existe no taxon_df
      if (acc_pos[1] <= nrow(taxon_df)) {
        return(list(id = taxon_df$id[acc_pos[1]],
                    name = taxon_df$taxonName[acc_pos[1]]))
      }
    }

    warning(paste("Accepted ID", acc_id, "not found in id_lookup for row", row_idx),
            call. = FALSE)
    return(list(id = NA_character_, name = NA_character_))

  } else if (!is.na(status) && status == "NOME_ACEITO") {
    return(list(id = taxon_df$id[row_idx],
                name = taxon_df$taxonName[row_idx]))
  }

  list(id = NA_character_, name = NA_character_)
}


#_______________________________________________________________________________
# Build an NA-filled result row for an unmatched input name ####
.funga_na_row <- function(spname, include_correct = FALSE) {
  df <- data.frame(
    Search = spname,
    FFB.taxon.ID = NA_character_,
    taxonRank = NA_character_,
    Input.InfraspecificEpithet = NA_character_,
    scientificNameAuthorship = NA_character_,
    taxonomicStatus = NA_character_,
    Accepted.taxon.ID = NA_character_,
    Accepted.taxon.Name = NA_character_,
    family = NA_character_,
    order = NA_character_,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (include_correct) {
    df$Correct.Spelling <- NA
    df <- df[, c("Search", "Correct.Spelling",
                 names(df)[!names(df) %in% c("Search", "Correct.Spelling")])]
  }

  df
}

#_______________________________________________________________________________
# Build result row(s) from one or more matched row indices in taxon_df. ####
.funga_build_rows <- function(spname, rows, dists, taxon_df, id_lookup,
                              include_distance = FALSE,
                              include_correct = FALSE) {

  acc_list <- tryCatch({
    lapply(rows, .funga_resolve_accepted,
           taxon_df = taxon_df, id_lookup = id_lookup)
  }, error = function(e) {
    warning(paste("Error resolving accepted name for", spname, ":", e$message),
            call. = FALSE)
    lapply(rows, function(x) list(id = NA_character_, name = NA_character_))
  })

  df <- data.frame(
    Search = spname,
    FFB.taxon.ID = taxon_df$id[rows],
    taxonRank = taxon_df$taxonRank[rows],
    Input.InfraspecificEpithet = .funga_get_col(taxon_df, "infraspecificEpithet")[rows],
    scientificNameAuthorship = .funga_get_col(taxon_df, "scientificNameAuthorship")[rows],
    taxonomicStatus = taxon_df$taxonomicStatus[rows],
    Accepted.taxon.ID = vapply(acc_list, function(x) {
      val <- x$id
      if (is.null(val) || is.na(val)) NA_character_ else as.character(val)
    }, character(1)),
    Accepted.taxon.Name = vapply(acc_list, `[[`, character(1), "name"),
    family = taxon_df$family[rows],
    order = .funga_get_col(taxon_df, "order")[rows],
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (include_distance) df$Name.Distance <- dists
  if (include_correct) {
    df$Correct.Spelling <- NA
    df <- df[, c("Search", "Correct.Spelling",
                 names(df)[!names(df) %in% c("Search", "Correct.Spelling")])]
  }

  df
}


#_______________________________________________________________________________
# Flag duplicated Accepted.taxon.Name entries in a result data.frame. ####
# Returns an integer vector: position of the first other occurrence, or NA.
.funga_find_dups <- function(result) {
  nms <- result$Accepted.taxon.Name
  dups <- rep(NA_integer_, nrow(result))
  for (i in seq_len(nrow(result))) {
    nm <- nms[i]
    if (!is.na(nm)) {
      hits <- which(nms == nm)
      if (length(hits) > 1L) dups[i] <- hits[hits != i][1L]
    }
  }
  dups
}


#_______________________________________________________________________________
# Download (if needed) and parse the FFB dataset, returning a list with the ####
# three pre-built structures shared by the search functions.
.funga_prepare_taxon <- function(version, verbose, rm_funga_database) {
  funga_download(version = version, dir = "funga_download", verbose = verbose)
  dwca <- funga_parse(path = "funga_download", version = version, verbose = verbose)
  taxon_df <- .funga_get_taxon(dwca)

  id_columns <- c("id", "acceptedNameUsageID", "parentNameUsageID", "originalNameUsageID")

  all_rows <- c()
  all_ids <- c()
  for (col in id_columns) {
    col_data <- taxon_df[[col]]
    valid_idx <- which(!is.na(col_data) & col_data != "")

    if (length(valid_idx) > 0) {
      all_rows <- c(all_rows, valid_idx)
      all_ids <- c(all_ids, as.character(col_data[valid_idx]))
    }
  }

  id_lookup <- tapply(all_rows, all_ids, unique, simplify = FALSE)

  # Remove the downloaded FFB folder funga_download
  if (rm_funga_database) {
    unlink("funga_download", recursive = TRUE)
  }

  list(
    taxon_df = taxon_df,
    genus_index = .funga_build_genus_index(taxon_df),
    id_lookup = id_lookup
  )
}


#_______________________________________________________________________________
# Core search loop shared by funga_search() and funga_match(). ####
.funga_search_impl <- function(splist, taxon_df, genus_index, id_lookup,
                               max_distance, genus_fuzzy,
                               show_correct, progress_bar) {

  splist_std <- .funga_names_standardize(splist)
  splist_class <- .funga_splist_classify(splist_std)

  n_sps <- length(splist)
  results <- vector("list", n_sps)
  homonyms <- logical(n_sps)
  is_exact <- logical(n_sps)

  if (progress_bar) pb <- utils::txtProgressBar(min = 0, max = n_sps, style = 3)

  for (i in seq_len(n_sps)) {
    sc <- splist_class[i, ]

    if (is.na(sc$epithet)) {
      warning(paste0("'", splist[i],
                     "' does not include an epithet and will be skipped."),
              call. = FALSE)
      results[[i]] <- .funga_na_row(splist[i], include_correct = show_correct)
      if (progress_bar) utils::setTxtProgressBar(pb, i)
      next
    }

    match_res <- .funga_search_ind(sc, taxon_df, genus_index,
                                   max_distance, genus_fuzzy)

    if (is.null(match_res)) {
      warning(paste0("No match found for '", splist[i], "'."), call. = FALSE)
      results[[i]] <- .funga_na_row(splist[i], include_correct = show_correct)
      if (progress_bar) utils::setTxtProgressBar(pb, i)
      next
    }

    is_exact[i] <- match_res$exact
    rows <- match_res$rows
    dists <- match_res$distances

    if (length(rows) > 1L) {
      homonyms[i] <- TRUE
      acc_idx <- rows[taxon_df$taxonomicStatus[rows] == "NOME_ACEITO"]
      chosen <- if (length(acc_idx) > 0L) acc_idx[1L] else rows[1L]
    } else {
      chosen <- rows[1L]
    }

    d_chosen <- dists[match(chosen, rows)]
    results[[i]] <- .funga_build_rows(spname = splist[i],
                                      rows = chosen,
                                      dists = d_chosen,
                                      taxon_df,
                                      id_lookup,
                                      include_distance = FALSE,
                                      include_correct = show_correct)
    if (progress_bar) utils::setTxtProgressBar(pb, i)
  }

  if (progress_bar) close(pb)

  # Verify whether all lines have the same column
  result_final <- do.call(rbind, results)
  rownames(result_final) <- NULL

  if (all(is.na(result_final$FFB.taxon.ID))) {
    warning("No match found for any input name. Try increasing 'max_distance'.")
    return(NULL)
  }

  if (show_correct) {
    result_final$Correct.Spelling <- is_exact
    result_final <- result_final[, c("Search", "Correct.Spelling",
                                     names(result_final)[!names(result_final) %in% c("Search", "Correct.Spelling")])]
  }

  if (any(homonyms)) {
    warning(paste0(
      "More than one name was matched for some inputs. ",
      "Only the first accepted name was returned. "),
      call. = FALSE)
    attr(result_final, "matched_mult") <- splist[homonyms]
  }

  result_final
}
