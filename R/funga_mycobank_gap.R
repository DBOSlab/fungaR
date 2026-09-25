#' Find fungal species in MycoBank that are potentially Brazilian but missing from FFB
#'
#' @description
#' Given a fungal genus or order, cross-checks \href{https://www.mycobank.org}{MycoBank}'s
#' global nomenclatural database against the Flora e Funga do Brasil (FFB) checklist to
#' flag species-level names that exist in MycoBank but are not currently registered in
#' FFB. When \code{check_occurrence = TRUE} (default), each candidate name is further
#' cross-checked against \href{https://www.gbif.org}{GBIF} (and, best-effort,
#' \href{https://specieslink.net}{speciesLink}) for occurrence evidence from Brazil, so
#' that taxonomic experts reviewing the output spreadsheet can prioritize candidates that
#' already have independent evidence of occurring in the country.
#'
#' @details
#' MycoBank does not offer a public search API suited to bulk automation, but it does
#' publish a full, regularly-updated bulk export of every name in its database
#' (\url{https://www.mycobank.org/images/MBList.zip}). This function downloads and caches
#' that export locally (the same way \code{\link{funga_download}} caches the FFB dataset),
#' then filters it down to the requested genus or order using MycoBank's own
#' \code{Classification} column, rather than attempting to scrape MycoBank's live,
#' JavaScript-driven search interface.
#'
#' The FFB side of the comparison reuses \code{\link{funga_get_children_taxa}} to list every
#' species (accepted and synonym) currently registered in FFB for the requested genus/order.
#'
#' @usage
#' funga_mycobank_gap(
#'   taxon,
#'   rank = c("genus", "order"),
#'   check_occurrence = TRUE,
#'   max_check = 200,
#'   version = "latest",
#'   mycobank_dir = "mycobank_download",
#'   verbose = TRUE,
#'   save = TRUE,
#'   dir = "funga_mycobank_gap",
#'   filename = NULL,
#'   html_report = TRUE,
#'   open_report = interactive()
#' )
#'
#' @param taxon Character. A single fungal genus or order name (e.g. \code{"Trichoderma"}
#'   or \code{"Xylariales"}).
#'
#' @param rank Character. Whether \code{taxon} is a \code{"genus"} (default) or an
#'   \code{"order"}.
#'
#' @param check_occurrence Logical. If \code{TRUE} (default), each MycoBank name missing
#'   from FFB is additionally queried against GBIF (and speciesLink, best-effort) for
#'   Brazil-only occurrence records, adding evidence columns to help prioritize which
#'   candidates are worth a taxonomist's manual review. Set to \code{FALSE} to skip this
#'   (much faster, but without occurrence evidence).
#'
#' @param max_check Numeric. Safety cap on how many missing candidates are sent through the
#'   (comparatively slow, one-request-per-name) occurrence check when
#'   \code{check_occurrence = TRUE}. Defaults to \code{200}; if more candidates are found,
#'   only the first \code{max_check} (alphabetically) are checked and a warning is issued.
#'   Ignored when \code{check_occurrence = FALSE}.
#'
#' @param version Character. FFB dataset version to compare against. Defaults to
#'   \code{"latest"}. Passed to \code{\link{funga_get_children_taxa}}.
#'
#' @param mycobank_dir Character. Local directory used to cache the downloaded MycoBank
#'   bulk export. Defaults to \code{"mycobank_download"}. Reused on subsequent calls to
#'   avoid re-downloading the (~140MB) file.
#'
#' @param verbose Logical. If \code{TRUE} (default), prints progress messages.
#'
#' @param save Logical. If \code{TRUE} (default), the result is saved to disk as an
#'   \code{.xlsx} spreadsheet.
#'
#' @param dir Character. Directory where the spreadsheet is saved when \code{save = TRUE}.
#'   Defaults to \code{"funga_mycobank_gap"}.
#'
#' @param filename Character. Name of the \code{.xlsx} file (without extension) to save.
#'   Defaults to \code{"funga_mycobank_gap_<taxon>"}.
#'
#' @param html_report Logical. If \code{TRUE} (default), also writes a self-contained
#'   HTML report (\code{<dir>/<filename>.html}) summarizing the results: KPI counts,
#'   a families-affected breakdown, and the full candidate table as a sortable,
#'   filterable \pkg{DT} widget with buttons to copy or download it as CSV/Excel.
#'   Requires the \pkg{rmarkdown}, \pkg{DT}, and \pkg{htmltools} packages; if any is
#'   missing, the report is skipped with a message (the \code{.xlsx} spreadsheet is
#'   unaffected).
#'
#' @param open_report Logical. If \code{TRUE} (default in interactive sessions), opens
#'   the rendered HTML report in the default browser.
#'
#' @return A \code{data.frame}, one row per MycoBank species-level name found in the
#'   requested genus/order that is absent from the current FFB checklist, with columns:
#' \describe{
#'   \item{MycoBank_Number}{MycoBank's numeric identifier for the name.}
#'   \item{Taxon_name}{The binomial as registered in MycoBank.}
#'   \item{Authors}{Full author citation.}
#'   \item{Name_status}{MycoBank's nomenclatural status (e.g. \code{"Legitimate"}).}
#'   \item{Classification}{Full MycoBank classification string (Kingdom to genus).}
#'   \item{Current_name}{MycoBank's currently accepted name for this entry, when different
#'     (i.e. when \code{Taxon_name} is itself a synonym).}
#'   \item{MycoBank_URL}{Direct link to the name's MycoBank page.}
#'   \item{GBIF_Brazil_records, GBIF_Brazil_states}{Occurrence count and states with GBIF
#'     records from Brazil (only when \code{check_occurrence = TRUE}).}
#'   \item{speciesLink_Brazil_records, speciesLink_Brazil_states}{Same, from speciesLink,
#'     best-effort (see Note).}
#'   \item{In_FFB}{Always \code{FALSE} (kept for clarity when combining with other tables).}
#' }
#'
#' @note
#' speciesLink's public API endpoint has, at the time of writing, an SSL certificate that
#' does not match its own hostname, which causes standard HTTPS clients (including R's) to
#' refuse the connection. This function therefore treats speciesLink evidence as strictly
#' best-effort: if the request fails for any reason, the corresponding columns are simply
#' \code{NA} rather than aborting the function. GBIF is the dependable primary evidence
#' source (and already aggregates a large share of Brazilian collections, including many
#' also published through speciesLink).
#'
#' @seealso \code{\link{funga_distribution_gap}}, \code{\link{funga_get_children_taxa}}
#'
#' @author
#' Domingos Cardoso
#'
#' @examples
#' \dontrun{
#' # Species-level names in MycoBank for genus Trichoderma missing from FFB,
#' # with Brazil-occurrence evidence from GBIF/speciesLink
#' gap <- funga_mycobank_gap(taxon = "Trichoderma", rank = "genus")
#'
#' # Faster, without the occurrence cross-check
#' gap_fast <- funga_mycobank_gap(taxon = "Trichoderma", rank = "genus",
#'                                check_occurrence = FALSE)
#' }
#'
#' @importFrom utils download.file unzip URLencode
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_xlsx
#' @importFrom jsonlite fromJSON
#'
#' @export

funga_mycobank_gap <- function(taxon,
                               rank = c("genus", "order"),
                               check_occurrence = TRUE,
                               max_check = 200,
                               version = "latest",
                               mycobank_dir = "mycobank_download",
                               verbose = TRUE,
                               save = TRUE,
                               dir = "funga_mycobank_gap",
                               filename = NULL,
                               html_report = TRUE,
                               open_report = interactive()) {

  if (missing(taxon) || is.null(taxon) || !is.character(taxon) || length(taxon) != 1) {
    stop("'taxon' must be a single character string (one genus or order name).",
         call. = FALSE)
  }
  rank <- match.arg(rank)
  taxon <- trimws(taxon)

  if (is.null(filename)) {
    filename <- paste0("funga_mycobank_gap_", gsub("\\s+", "_", taxon))
  }

  # ------------------------------------------------------------------------
  # 1. FFB side: species (accepted + synonyms) already registered for taxon
  # ------------------------------------------------------------------------
  if (verbose) message("Retrieving the current FFB checklist for '", taxon, "'...")

  ffb_species <- tryCatch({
    funga_get_children_taxa(taxon_name = taxon, rank = rank, child_rank = "species",
                            include_synonyms = TRUE, version = version, verbose = FALSE)
  }, error = function(e) {
    if (verbose) {
      message("  '", taxon, "' was not found in the current FFB checklist ",
             "(treating FFB's known species list as empty).")
    }
    data.frame()
  })

  ffb_names <- if (nrow(ffb_species) > 0) unique(ffb_species$taxonName) else character(0)

  # ------------------------------------------------------------------------
  # 2. MycoBank side: download/cache the bulk export, then filter
  # ------------------------------------------------------------------------
  mycobank_dir <- .arg_check_dir(mycobank_dir)
  mb_file <- file.path(mycobank_dir, "MBList.xlsx")

  if (!file.exists(mb_file)) {
    if (!dir.exists(mycobank_dir)) dir.create(mycobank_dir, recursive = TRUE)

    if (verbose) {
      message("Downloading MycoBank's bulk name list (~140MB, cached for future calls)...")
    }
    destzipfile <- tempfile(fileext = ".zip")
    utils::download.file(url = "https://www.mycobank.org/images/MBList.zip",
                         destfile = destzipfile, method = "curl")
    utils::unzip(destzipfile, exdir = mycobank_dir)
    unlink(destzipfile)
  } else if (verbose) {
    message("Using previously downloaded MycoBank list in '", mycobank_dir, "'.")
  }

  if (verbose) message("Reading and filtering the MycoBank list...")
  mb <- readxl::read_xlsx(mb_file, sheet = "Sheet1")

  is_species <- mb$Rank %in% "sp."
  if (rank == "genus") {
    in_taxon <- grepl(paste0("^", taxon, " "), mb[["Taxon name"]])
  } else {
    classif_tokens <- strsplit(mb$Classification, ",\\s*")
    in_taxon <- vapply(classif_tokens, function(x) taxon %in% x, logical(1))
  }
  mb_sub <- mb[is_species & in_taxon & !is.na(mb[["Taxon name"]]), ]

  if (verbose) {
    message("  Found ", nrow(mb_sub), " species-level name(s) in MycoBank for ",
           rank, " '", taxon, "'.")
  }

  # ------------------------------------------------------------------------
  # 3. Names in MycoBank but absent from the FFB checklist
  # ------------------------------------------------------------------------
  # Comparing against FFB by the MycoBank name's *currently accepted* name
  # (falling back to the name itself when it has none) avoids flagging every
  # obscure historical synonym of a species FFB already lists under a
  # different name - only genuinely un-represented accepted species are kept.
  effective_name <- ifelse(!is.na(mb_sub[["Current name"]]) & nzchar(mb_sub[["Current name"]]),
                           mb_sub[["Current name"]], mb_sub[["Taxon name"]])
  missing <- mb_sub[!effective_name %in% ffb_names, ]
  missing_effective_name <- effective_name[!effective_name %in% ffb_names]

  # One row per genuinely missing accepted species: prefer the row where the
  # MycoBank name IS the accepted name (Taxon name == Current name / has no
  # Current name), falling back to the first synonym row otherwise.
  is_accepted_row <- missing[["Taxon name"]] == missing_effective_name |
    is.na(missing[["Current name"]]) | !nzchar(missing[["Current name"]])
  order_pref <- order(missing_effective_name, !is_accepted_row)
  missing <- missing[order_pref, ][!duplicated(missing_effective_name[order_pref]), ]
  missing <- missing[order(missing[["Taxon name"]]), ]

  if (verbose) {
    message("  ", nrow(missing), " of those resolve to a species not currently in FFB.")
  }

  result <- data.frame(
    MycoBank_Number = missing[["MycoBank #"]],
    Taxon_name = missing[["Taxon name"]],
    Authors = missing[["Authors"]],
    Name_status = missing[["Name status"]],
    Classification = missing[["Classification"]],
    Current_name = missing[["Current name"]],
    MycoBank_URL = missing[["Hyperlink to MB"]],
    In_FFB = FALSE,
    stringsAsFactors = FALSE
  )

  # ------------------------------------------------------------------------
  # 4. Optional Brazil-occurrence cross-check (GBIF primary, speciesLink best-effort)
  # ------------------------------------------------------------------------
  if (check_occurrence && nrow(result) > 0) {

    if (nrow(result) > max_check) {
      warning(sprintf(
        "%d candidate names found, but only the first %d (of 'max_check') will be ",
        nrow(result), max_check), "checked for Brazil occurrence evidence; ",
        "increase 'max_check' to check them all.", call. = FALSE)
      check_rows <- seq_len(max_check)
    } else {
      check_rows <- seq_len(nrow(result))
    }

    result$GBIF_Brazil_records <- NA_integer_
    result$GBIF_Brazil_states <- NA_character_
    result$speciesLink_Brazil_records <- NA_integer_
    result$speciesLink_Brazil_states <- NA_character_

    for (i in check_rows) {
      if (verbose) {
        message(sprintf("  Checking Brazil occurrence %d/%d: %s",
                        i, length(check_rows), result$Taxon_name[i]))
      }
      gbif <- .gbif_brazil_occurrence(result$Taxon_name[i])
      splink <- .splink_brazil_occurrence(result$Taxon_name[i])

      result$GBIF_Brazil_records[i] <- gbif$n_records
      result$GBIF_Brazil_states[i] <- gbif$states
      result$speciesLink_Brazil_records[i] <- splink$n_records
      result$speciesLink_Brazil_states[i] <- splink$states
    }
  }

  rownames(result) <- NULL

  if (verbose) {
    message(sprintf("\n\u2713 %d candidate species found in MycoBank but missing from FFB",
                    nrow(result)))
  }

  if (save && nrow(result) > 0) {
    dir <- .arg_check_dir(dir)
    .save_xlsx(result, verbose = verbose, filename = filename, dir = dir)
  }

  if (html_report && nrow(result) > 0) {
    extract_family <- function(classification) {
      toks <- strsplit(classification, ",\\s*")[[1]]
      fam <- toks[grepl("aceae$", toks)]
      if (length(fam) == 0) NA_character_ else fam[length(fam)]
    }
    family_vec <- vapply(result$Classification, extract_family, character(1),
                         USE.NAMES = FALSE)
    family_gap <- as.data.frame(table(family_vec[!is.na(family_vec)]),
                                stringsAsFactors = FALSE)
    if (nrow(family_gap) > 0) {
      names(family_gap) <- c("family", "n_missing")
      family_gap <- family_gap[order(-family_gap$n_missing), ]
      rownames(family_gap) <- NULL
    }

    report_data <- list(
      taxon = taxon,
      rank = rank,
      n_mycobank = nrow(mb_sub),
      n_in_ffb = length(ffb_names),
      n_missing = nrow(result),
      n_with_evidence = if (check_occurrence) {
        sum(!is.na(result$GBIF_Brazil_records) & result$GBIF_Brazil_records > 0, na.rm = TRUE)
      } else {
        NULL
      },
      family_gap = family_gap,
      result = result
    )

    dir <- .arg_check_dir(dir)
    .funga_render_report(template = "funga_mycobank_gap_report.Rmd",
                         data_list = report_data,
                         taxon = taxon,
                         dir = dir,
                         filename = filename,
                         verbose = verbose,
                         open_report = open_report)
  }

  return(result)
}
