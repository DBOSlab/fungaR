#' Find species with Index Fungorum Brazil evidence that are missing from FFB
#'
#' @description
#' Given a fungal genus, cross-checks \href{https://www.speciesfungorum.org}{Species
#' Fungorum} (Index Fungorum, maintained by the Royal Botanic Gardens, Kew) against the
#' Flora e Funga do Brasil (FFB) checklist to find species-level names that Index
#' Fungorum's own reported locality places in Brazil but that are not currently
#' registered in FFB. Like \code{\link{mycobank_gap}}, this is not a general diff
#' of the two databases: a name missing from FFB is only returned when Index Fungorum's
#' own locality data indicates it occurs in Brazil.
#'
#' @details
#' Species Fungorum/Index Fungorum has no bulk export, but it does publish a public,
#' documented web service (\url{https://www.indexfungorum.org/ixfwebservice/fungus.asmx})
#' that answers plain HTTP GET requests with XML - no SOAP client, headless browser, or
#' authentication required, and no per-record page visit: a single request returns every
#' name matching \code{taxon}, including, for many records, the type specimen's own
#' reported \code{LOCATION} and \code{HOST} fields, which is what
#' \code{Fungorum_Brazil_Evidence} is derived from. Index Fungorum's \code{LOCATION}
#' field very often names a Brazilian state directly (e.g. \code{"Pernambuco"}) rather
#' than the country itself, so the check covers the country name and every Brazilian
#' state name (with or without diacritics), not just the literal word "Brazil".
#'
#' A candidate counts as "already in FFB" if either its raw Index Fungorum name or its
#' Index Fungorum \emph{currently accepted} name matches an FFB species - not the
#' current name alone - because Index Fungorum and FFB sometimes disagree on which genus
#' a name currently belongs to (the same situation documented for
#' \code{\link{mycobank_gap}}).
#'
#' The FFB side of the comparison reuses \code{\link{funga_get_children_taxa}} to list
#' every species (accepted and synonym) currently registered in FFB for the requested
#' genus.
#'
#' @usage
#' spfungorum_gap(
#'   taxon,
#'   require_brazil_evidence = TRUE,
#'   max_number = 1000,
#'   version = "latest",
#'   verbose = TRUE,
#'   save = TRUE,
#'   dir = "spfungorum_gap",
#'   filename = NULL,
#'   html_report = TRUE,
#'   open_report = interactive()
#' )
#'
#' @param taxon Character. A single fungal genus name (e.g. \code{"Phellinotus"}).
#'   Species Fungorum's web service has no operation for searching by higher
#'   classification, so order-level searches are not supported.
#'
#' @param require_brazil_evidence Logical. If \code{TRUE} (default), the returned table
#'   is filtered to only the candidates whose Index Fungorum \code{Location} mentions
#'   Brazil (the country name or any Brazilian state) - i.e. names Index Fungorum itself
#'   indicates occur in Brazil but that FFB is still missing. Set to \code{FALSE} to
#'   instead return every Index Fungorum species-level name missing from FFB regardless
#'   of locality, with \code{Fungorum_Brazil_Evidence} alongside for manual review.
#'
#' @param max_number Numeric. Maximum number of matching names requested from the web
#'   service in a single call. Defaults to \code{1000}, generous for any genus.
#'
#' @param version Character. FFB dataset version to compare against. Defaults to
#'   \code{"latest"}. Passed to \code{\link{funga_get_children_taxa}}.
#'
#' @param verbose Logical. If \code{TRUE} (default), prints progress messages.
#'
#' @param save Logical. If \code{TRUE} (default), the result is saved to disk as an
#'   \code{.xlsx} spreadsheet.
#'
#' @param dir Character. Directory where the spreadsheet is saved when \code{save = TRUE}.
#'   Defaults to \code{"spfungorum_gap"}.
#'
#' @param filename Character. Name of the \code{.xlsx} file (without extension) to save.
#'   Defaults to \code{"spfungorum_gap_<taxon>"}.
#'
#' @param html_report Logical. If \code{TRUE} (default), also writes a self-contained
#'   HTML report (\code{<dir>/<filename>.html}) summarizing the results: KPI counts and
#'   the full candidate table as a sortable, filterable \pkg{DT} widget with buttons to
#'   copy or download it as CSV/Excel. Requires the \pkg{rmarkdown}, \pkg{DT}, and
#'   \pkg{htmltools} packages; if any is missing, the report is skipped with a message
#'   (the \code{.xlsx} spreadsheet is unaffected).
#'
#' @param open_report Logical. If \code{TRUE} (default in interactive sessions), opens
#'   the rendered HTML report in the default browser.
#'
#' @return A \code{data.frame}, one row per Index Fungorum species-level name found in
#'   the requested genus that is absent from the current FFB checklist and, by default,
#'   whose Index Fungorum locality data indicates it occurs in Brazil, with columns:
#' \describe{
#'   \item{Fungorum_Number}{Index Fungorum's numeric record identifier for the name.}
#'   \item{Taxon_name}{The binomial as registered in Index Fungorum.}
#'   \item{Authors}{Author citation.}
#'   \item{Year}{Year of publication.}
#'   \item{Name_status}{Index Fungorum's nomenclatural status (e.g. \code{"Legitimate"}).}
#'   \item{Current_name}{Index Fungorum's currently accepted name for this entry, when
#'     different (i.e. when \code{Taxon_name} is itself a synonym).}
#'   \item{Fungorum_Location, Fungorum_Host}{Type specimen locality and host, as reported
#'     in Index Fungorum's own record (may be \code{NA} - not every record has these).}
#'   \item{Fungorum_URL}{Direct link to the name's Species Fungorum page.}
#'   \item{Fungorum_Brazil_Evidence}{Logical; \code{TRUE} when \code{Fungorum_Location}
#'     mentions Brazil.}
#'   \item{In_FFB}{Always \code{FALSE} (kept for clarity when combining with other tables).}
#' }
#'
#' @seealso \code{\link{spfungorum_records}}, \code{\link{mycobank_gap}},
#'   \code{\link{funga_get_children_taxa}}
#'
#' @author
#' Domingos Cardoso
#'
#' @examples
#' \dontrun{
#' # Index Fungorum species-level names for genus Phellinotus with a Brazilian
#' # reported locality that are still missing from FFB
#' gap <- spfungorum_gap(taxon = "Phellinotus")
#'
#' # Every Index Fungorum name missing from FFB regardless of locality, for manual review
#' gap_all <- spfungorum_gap(taxon = "Phellinotus", require_brazil_evidence = FALSE)
#' }
#'
#' @importFrom xml2 read_xml xml_find_all xml_find_first xml_text
#' @importFrom openxlsx write.xlsx
#'
#' @export

spfungorum_gap <- function(taxon,
                               require_brazil_evidence = TRUE,
                               max_number = 1000,
                               version = "latest",
                               verbose = TRUE,
                               save = TRUE,
                               dir = "spfungorum_gap",
                               filename = NULL,
                               html_report = TRUE,
                               open_report = interactive()) {

  if (missing(taxon) || is.null(taxon) || !is.character(taxon) || length(taxon) != 1) {
    stop("'taxon' must be a single character string (one genus name).",
         call. = FALSE)
  }
  taxon <- trimws(taxon)

  if (is.null(filename)) {
    filename <- paste0("spfungorum_gap_", gsub("\\s+", "_", taxon))
  }

  # ------------------------------------------------------------------------
  # 1. FFB side: species (accepted + synonyms) already registered for taxon
  # ------------------------------------------------------------------------
  if (verbose) message("Retrieving the current FFB checklist for '", taxon, "'...")

  ffb_species <- tryCatch({
    funga_get_children_taxa(taxon_name = taxon, rank = "genus", child_rank = "species",
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
  # 2. Index Fungorum side: query the web service
  # ------------------------------------------------------------------------
  if (verbose) message("Querying Index Fungorum's web service for genus '", taxon, "'...")

  fg <- .indexfungorum_name_search(taxon, rank = "genus", max_number = max_number)

  if (verbose) {
    message("  Found ", nrow(fg), " species-level name(s) in Index Fungorum for genus '",
           taxon, "'.")
  }

  # ------------------------------------------------------------------------
  # 3. Names in Index Fungorum but absent from the FFB checklist
  # ------------------------------------------------------------------------
  # A candidate counts as "already in FFB" if EITHER its raw Index Fungorum name OR
  # its currently accepted name matches an FFB species - see mycobank_gap()
  # for why checking only the current name is not enough (Index Fungorum and FFB
  # can disagree on which genus a name currently belongs to).
  already_in_ffb <- fg$Taxon_name %in% ffb_names | fg$Current_name %in% ffb_names
  missing <- fg[!already_in_ffb, ]
  rownames(missing) <- NULL

  if (verbose) {
    message("  ", nrow(missing), " of those resolve to a species not currently in FFB.")
  }

  n_missing_any_locality <- nrow(missing)

  result <- data.frame(
    Fungorum_Number = missing$Fungorum_Number,
    Taxon_name = missing$Taxon_name,
    Authors = missing$Authors,
    Year = missing$Year,
    Name_status = missing$Name_status,
    Current_name = missing$Current_name,
    Fungorum_Location = missing$Location,
    Fungorum_Host = missing$Host,
    Fungorum_URL = ifelse(is.na(missing$Fungorum_Number), NA_character_,
                          paste0("https://www.speciesfungorum.org/names/NamesRecord.asp?RecordID=",
                                missing$Fungorum_Number)),
    In_FFB = logical(nrow(missing)),
    stringsAsFactors = FALSE
  )
  result$Fungorum_Brazil_Evidence <- vapply(result$Fungorum_Location,
                                            .location_mentions_brazil, logical(1))

  if (require_brazil_evidence) {
    n_brazil <- sum(result$Fungorum_Brazil_Evidence, na.rm = TRUE)
    if (verbose) {
      message(sprintf("  %d of %d missing candidate(s) have Index Fungorum locality evidence for Brazil.",
                      n_brazil, nrow(result)))
    }
    result <- result[result$Fungorum_Brazil_Evidence, ]
  }

  result <- result[order(result$Taxon_name), ]
  rownames(result) <- NULL

  if (verbose) {
    message(sprintf("\n\u2713 %d candidate species found in Index Fungorum but missing from FFB",
                    nrow(result)))
  }

  if (save && nrow(result) > 0) {
    dir <- .arg_check_dir(dir)
    .save_xlsx(result, verbose = verbose, filename = filename, dir = dir)
  }

  if (html_report && nrow(result) > 0) {
    brazil_filtered <- require_brazil_evidence

    report_data <- list(
      taxon = taxon,
      rank = "genus",
      n_fungorum = nrow(fg),
      n_in_ffb = length(ffb_names),
      n_missing_any_locality = n_missing_any_locality,
      n_missing = nrow(result),
      brazil_filtered = brazil_filtered,
      result = result
    )

    dir <- .arg_check_dir(dir)
    .funga_render_report(template = "spfungorum_gap_report.Rmd",
                         data_list = report_data,
                         taxon = taxon,
                         dir = dir,
                         filename = filename,
                         verbose = verbose,
                         open_report = open_report)
  }

  return(result)
}
