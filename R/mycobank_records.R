#' Retrieve MycoBank records for a taxon, filtered to Brazilian occurrence evidence
#'
#' @description
#' Given a fungal species, genus, or order, retrieves every matching
#' \href{https://www.mycobank.org}{MycoBank} name and, by default, filters the result
#' down to only the names whose own MycoBank locality data places them in Brazil -
#' returning a structured spreadsheet of all available MycoBank information (including
#' each name's original MycoBank URL) for those Brazilian records. Unlike
#' \code{\link{mycobank_gap}}, this function does not cross-check FFB at all: it
#' is a straightforward MycoBank retrieval tool, not a gap/comparison one.
#'
#' @details
#' MycoBank does not offer a public search API suited to bulk automation, but it does
#' publish a full, regularly-updated bulk export of every name in its database
#' (\url{https://www.mycobank.org/images/MBList.zip}). This function downloads and caches
#' that export locally (the same way \code{\link{funga_download}} caches the FFB dataset),
#' then filters it down to the requested species, genus, or order.
#'
#' The bulk export itself carries no locality information, so determining Brazil evidence
#' requires visiting each matching name's individual MycoBank page (its
#' \code{MycoBank_URL}) to read the "Location details" (and "Host/Substrate") reported for
#' its type specimen. MycoBank's name pages are a JavaScript single-page application with
#' no static HTML fallback, so this step (\code{check_locality = TRUE}, the default)
#' requires the \pkg{chromote} package (and a local Chrome/Chromium installation) to drive
#' a headless browser. When \code{require_brazil_evidence = TRUE} (the default), the
#' returned table is filtered down to only the names whose MycoBank locality mentions
#' Brazil; set it to \code{FALSE} to instead get every matching MycoBank name regardless
#' of locality (with the evidence columns alongside, for manual review). Brazil-evidence
#' filtering has no effect if \code{check_locality = FALSE}, since no locality data is
#' available to filter on; a message explains this when it happens.
#'
#' @usage
#' mycobank_records(
#'   taxon,
#'   rank = c("species", "genus", "order"),
#'   check_locality = TRUE,
#'   require_brazil_evidence = TRUE,
#'   max_check = 200,
#'   mycobank_dir = "mycobank_download",
#'   verbose = TRUE,
#'   save = TRUE,
#'   dir = "mycobank_records",
#'   filename = NULL,
#'   html_report = TRUE,
#'   open_report = interactive()
#' )
#'
#' @param taxon Character. A single fungal species, genus, or order name (e.g.
#'   \code{"Trichoderma harzianum"}, \code{"Trichoderma"}, or \code{"Xylariales"}).
#'
#' @param rank Character. Whether \code{taxon} is a \code{"species"} (default), a
#'   \code{"genus"}, or an \code{"order"}.
#'
#' @param check_locality Logical. If \code{TRUE} (default), each matching MycoBank name
#'   has its own MycoBank name page visited (via \pkg{chromote}) to read the type
#'   specimen's reported locality and host/substrate, determining which names MycoBank
#'   itself already associates with Brazil. Set to \code{FALSE} to skip this (much
#'   faster, but without locality evidence, and \code{require_brazil_evidence} has no
#'   effect).
#'
#' @param require_brazil_evidence Logical. If \code{TRUE} (default), the returned table
#'   is filtered to only the names whose MycoBank locality mentions Brazil. Set to
#'   \code{FALSE} to instead return every matching MycoBank name regardless of locality
#'   (with \code{MycoBank_Brazil_Evidence} alongside for manual review). Only meaningful
#'   when \code{check_locality = TRUE}.
#'
#' @param max_check Numeric. Safety cap on how many matching names are sent through the
#'   (comparatively slow, one-page-load-per-name) locality check when
#'   \code{check_locality = TRUE}. Defaults to \code{200}; if more names are found, only
#'   the first \code{max_check} (alphabetically) are checked and a warning is issued.
#'   Ignored when \code{check_locality = FALSE}.
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
#'   Defaults to \code{"mycobank_records"}.
#'
#' @param filename Character. Name of the \code{.xlsx} file (without extension) to save.
#'   Defaults to \code{"mycobank_records_<taxon>"}.
#'
#' @param html_report Logical. If \code{TRUE} (default), also writes a self-contained
#'   HTML report (\code{<dir>/<filename>.html}) summarizing the results: KPI counts,
#'   a families-affected breakdown, and the full record table as a sortable,
#'   filterable \pkg{DT} widget with buttons to copy or download it as CSV/Excel.
#'   Requires the \pkg{rmarkdown}, \pkg{DT}, and \pkg{htmltools} packages; if any is
#'   missing, the report is skipped with a message (the \code{.xlsx} spreadsheet is
#'   unaffected).
#'
#' @param open_report Logical. If \code{TRUE} (default in interactive sessions), opens
#'   the rendered HTML report in the default browser.
#'
#' @return A \code{data.frame}, one row per matching MycoBank name and, by default, with
#'   a Brazilian MycoBank locality, with columns:
#' \describe{
#'   \item{MycoBank_Number}{MycoBank's numeric identifier for the name.}
#'   \item{Taxon_name}{The binomial as registered in MycoBank.}
#'   \item{Authors}{Full author citation.}
#'   \item{Year}{Year of effective publication.}
#'   \item{Name_status}{MycoBank's nomenclatural status (e.g. \code{"Legitimate"}).}
#'   \item{Classification}{Full MycoBank classification string (Kingdom to genus).}
#'   \item{Current_name}{MycoBank's currently accepted name for this entry, when different
#'     (i.e. when \code{Taxon_name} is itself a synonym).}
#'   \item{Synonymy}{MycoBank's full synonymy text (current name and/or basionym, each with
#'     its complete author/journal/year citation), from the bulk export.}
#'   \item{MycoBank_URL}{Direct link to the name's MycoBank page.}
#'   \item{MycoBank_Locality, MycoBank_Substrate}{Type specimen locality and host/substrate
#'     as reported on the name's own MycoBank page (only when \code{check_locality = TRUE}).}
#'   \item{MycoBank_Etymology}{The name's etymology, as reported on its own MycoBank page
#'     (only when \code{check_locality = TRUE}).}
#'   \item{MycoBank_Name_Type}{MycoBank's nomenclatural name type for this entry (e.g.
#'     \code{"Basionym"}, \code{"Combination"}; only when \code{check_locality = TRUE}).}
#'   \item{MycoBank_Type_Specimen}{The type specimen voucher (e.g. \code{"URM 80362
#'     holotype"}; only when \code{check_locality = TRUE}).}
#'   \item{MycoBank_Collector}{The type specimen's collector, as reported on the name's own
#'     MycoBank page (only when \code{check_locality = TRUE}).}
#'   \item{MycoBank_Protolog}{The original-publication (protolog) citation, as reported on
#'     the name's own MycoBank page (only when \code{check_locality = TRUE}).}
#'   \item{MycoBank_Brazil_Evidence}{Logical; \code{TRUE} when \code{MycoBank_Locality}
#'     mentions Brazil (only when \code{check_locality = TRUE}).}
#' }
#'
#' @seealso \code{\link{mycobank_gap}}, \code{\link{distribution_gap}}
#'
#' @author
#' Domingos Cardoso
#'
#' @examples
#' \dontrun{
#' # All MycoBank information for a single species, only if MycoBank places
#' # it in Brazil
#' sp <- mycobank_records(taxon = "Trichoderma harzianum")
#'
#' # Every MycoBank species-level name in genus Trichoderma with a Brazilian
#' # MycoBank locality
#' genus_records <- mycobank_records(taxon = "Trichoderma", rank = "genus")
#'
#' # Every matching name regardless of locality, for manual review
#' all_records <- mycobank_records(taxon = "Trichoderma", rank = "genus",
#'                                       require_brazil_evidence = FALSE)
#' }
#'
#' @importFrom utils download.file unzip
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_xlsx
#'
#' @export

mycobank_records <- function(taxon,
                                   rank = c("species", "genus", "order"),
                                   check_locality = TRUE,
                                   require_brazil_evidence = TRUE,
                                   max_check = 200,
                                   mycobank_dir = "mycobank_download",
                                   verbose = TRUE,
                                   save = TRUE,
                                   dir = "mycobank_records",
                                   filename = NULL,
                                   html_report = TRUE,
                                   open_report = interactive()) {

  if (missing(taxon) || is.null(taxon) || !is.character(taxon) || length(taxon) != 1) {
    stop("'taxon' must be a single character string (one species, genus, or order name).",
         call. = FALSE)
  }
  rank <- match.arg(rank)
  taxon <- trimws(taxon)

  if (is.null(filename)) {
    filename <- paste0("mycobank_records_", gsub("\\s+", "_", taxon))
  }

  # ------------------------------------------------------------------------
  # 1. MycoBank side: download/cache the bulk export, then filter
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

  if (rank == "species") {
    is_species <- mb$Rank %in% "sp."
    in_taxon <- mb[["Taxon name"]] %in% taxon
  } else if (rank == "genus") {
    is_species <- mb$Rank %in% "sp."
    in_taxon <- grepl(paste0("^", taxon, " "), mb[["Taxon name"]])
  } else {
    is_species <- mb$Rank %in% "sp."
    classif_tokens <- strsplit(mb$Classification, ",\\s*")
    in_taxon <- vapply(classif_tokens, function(x) taxon %in% x, logical(1))
  }
  mb_sub <- mb[is_species & in_taxon & !is.na(mb[["Taxon name"]]), ]
  mb_sub <- mb_sub[order(mb_sub[["Taxon name"]]), ]

  if (verbose) {
    message("  Found ", nrow(mb_sub), " matching MycoBank name(s) for ",
           rank, " '", taxon, "'.")
  }

  result <- data.frame(
    MycoBank_Number = mb_sub[["MycoBank #"]],
    Taxon_name = mb_sub[["Taxon name"]],
    Authors = mb_sub[["Authors"]],
    Year = mb_sub[["Year of effective publication"]],
    Name_status = mb_sub[["Name status"]],
    Classification = mb_sub[["Classification"]],
    Current_name = mb_sub[["Current name"]],
    Synonymy = mb_sub[["Synonymy"]],
    MycoBank_URL = mb_sub[["Hyperlink to MB"]],
    stringsAsFactors = FALSE
  )

  # ------------------------------------------------------------------------
  # 2. MycoBank locality check (via chromote) and Brazil-evidence filtering
  # ------------------------------------------------------------------------
  n_any_locality <- nrow(result)

  if (check_locality && nrow(result) > 0) {

    if (!requireNamespace("chromote", quietly = TRUE)) {
      if (verbose) {
        message("  Package 'chromote' (and a local Chrome/Chromium install) is required ",
               "for the MycoBank locality check; skipping it. Install it with ",
               "install.packages('chromote').")
        if (require_brazil_evidence) {
          message("  'require_brazil_evidence = TRUE' has no effect without the locality ",
                 "check; returning all matching names instead.")
        }
      }
    } else {

      if (nrow(result) > max_check) {
        warning(sprintf(
          "%d matching name(s) found, but only the first %d (of 'max_check') will be ",
          nrow(result), max_check), "checked for locality; increase 'max_check' to ",
          "check them all.", call. = FALSE)
        check_rows <- seq_len(max_check)
      } else {
        check_rows <- seq_len(nrow(result))
      }

      result$MycoBank_Locality <- NA_character_
      result$MycoBank_Substrate <- NA_character_
      result$MycoBank_Etymology <- NA_character_
      result$MycoBank_Name_Type <- NA_character_
      result$MycoBank_Type_Specimen <- NA_character_
      result$MycoBank_Collector <- NA_character_
      result$MycoBank_Protolog <- NA_character_
      result$MycoBank_Brazil_Evidence <- NA

      session <- chromote::ChromoteSession$new()
      on.exit(try(session$close(), silent = TRUE), add = TRUE)

      for (i in check_rows) {
        if (verbose) {
          message(sprintf("  Checking MycoBank locality %d/%d: %s",
                          i, length(check_rows), result$Taxon_name[i]))
        }
        det <- .mycobank_page_details(session, result$MycoBank_URL[i],
                                      taxon_name = result$Taxon_name[i])
        result$MycoBank_Locality[i] <- det$locality
        result$MycoBank_Substrate[i] <- det$substrate
        result$MycoBank_Etymology[i] <- det$etymology
        result$MycoBank_Name_Type[i] <- det$name_type
        result$MycoBank_Type_Specimen[i] <- det$type_specimen
        result$MycoBank_Collector[i] <- det$collector
        result$MycoBank_Protolog[i] <- det$protolog
        result$MycoBank_Brazil_Evidence[i] <- !is.na(det$locality) &&
          grepl("brazil|brasil", det$locality, ignore.case = TRUE)
      }

      if (require_brazil_evidence) {
        n_brazil <- sum(result$MycoBank_Brazil_Evidence, na.rm = TRUE)
        if (verbose) {
          message(sprintf(
            "  %d of %d checked name(s) have MycoBank locality evidence for Brazil.",
            n_brazil, length(check_rows)))
        }
        result <- result[which(result$MycoBank_Brazil_Evidence), ]
      }
    }
  } else if (!check_locality && require_brazil_evidence && verbose && nrow(result) > 0) {
    message("  'require_brazil_evidence = TRUE' requires 'check_locality = TRUE' to have ",
           "MycoBank locality evidence to filter on; returning all matching names instead.")
  }

  rownames(result) <- NULL

  if (verbose) {
    message(sprintf("\n\u2713 %d MycoBank record(s) retrieved for %s '%s'",
                    nrow(result), rank, taxon))
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
      names(family_gap) <- c("family", "n_records")
      family_gap <- family_gap[order(-family_gap$n_records), ]
      rownames(family_gap) <- NULL
    }

    brazil_filtered <- require_brazil_evidence && check_locality &&
      "MycoBank_Brazil_Evidence" %in% names(result)

    report_data <- list(
      taxon = taxon,
      rank = rank,
      n_mycobank = nrow(mb_sub),
      n_any_locality = n_any_locality,
      n_records = nrow(result),
      brazil_filtered = brazil_filtered,
      family_gap = family_gap,
      result = result
    )

    dir <- .arg_check_dir(dir)
    .funga_render_report(template = "mycobank_records_report.Rmd",
                         data_list = report_data,
                         taxon = taxon,
                         dir = dir,
                         filename = filename,
                         verbose = verbose,
                         open_report = open_report)
  }

  return(result)
}
