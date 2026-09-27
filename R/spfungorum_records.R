#' Retrieve Species Fungorum (Index Fungorum) records for a taxon, filtered to Brazil evidence
#'
#' @description
#' Given a fungal species or genus, retrieves every matching name from
#' \href{https://www.speciesfungorum.org}{Species Fungorum} (the nomenclatural database
#' maintained by the Royal Botanic Gardens, Kew, also known as Index Fungorum) and, by
#' default, filters the result down to only the names whose own reported locality places
#' them in Brazil - returning a structured spreadsheet of Index Fungorum information
#' (including each name's original Species Fungorum URL) for those Brazilian records.
#' Like \code{\link{mycobank_records}}, this is a straightforward retrieval tool,
#' not a gap/comparison one - it does not cross-check FFB at all.
#'
#' @details
#' Species Fungorum/Index Fungorum has no bulk export, but it does publish a public,
#' documented web service (\url{https://www.indexfungorum.org/ixfwebservice/fungus.asmx})
#' that answers plain HTTP GET requests with XML - no SOAP client, headless browser, or
#' authentication required. A single request returns every name matching \code{taxon},
#' including, for many records, the type specimen's own reported \code{LOCATION} and
#' \code{HOST} fields - the data this function uses as its Brazil-evidence signal. Unlike
#' MycoBank, no per-record page visit is needed: everything comes back in one response.
#'
#' Index Fungorum's \code{LOCATION} field very often names a Brazilian state directly
#' (e.g. \code{"Pernambuco"}) rather than the country itself, so
#' \code{Fungorum_Brazil_Evidence} checks \code{Fungorum_Location} against the country
#' name and every Brazilian state name (with or without diacritics), not just the literal
#' word "Brazil".
#'
#' @usage
#' spfungorum_records(
#'   taxon,
#'   rank = c("species", "genus"),
#'   require_brazil_evidence = TRUE,
#'   fetch_details = TRUE,
#'   max_number = 1000,
#'   verbose = TRUE,
#'   save = TRUE,
#'   dir = "spfungorum_records",
#'   filename = NULL,
#'   html_report = TRUE,
#'   open_report = interactive()
#' )
#'
#' @param taxon Character. A single fungal species or genus name (e.g.
#'   \code{"Phellinotus neoaridus"} or \code{"Phellinotus"}).
#'
#' @param rank Character. Whether \code{taxon} is a \code{"species"} (default) or a
#'   \code{"genus"}. Species Fungorum's web service has no operation for searching by
#'   higher classification (order/family), so \code{rank = "order"} is not supported.
#'
#' @param require_brazil_evidence Logical. If \code{TRUE} (default), the returned table
#'   is filtered to only the names whose Index Fungorum \code{Location} mentions Brazil
#'   (the country name or any Brazilian state). Set to \code{FALSE} to instead return
#'   every matching name regardless of locality, for manual review.
#'
#' @param fetch_details Logical. If \code{TRUE} (default), each returned name's own
#'   Species Fungorum page is visited (a fast, plain HTTP request - no headless browser,
#'   unlike MycoBank) to read the journal it was published in, the full formatted
#'   citation, the basionym or typification details, the current name's own full
#'   citation, and the Index Fungorum LSID - none of which are in the bulk
#'   \code{NameSearch} response. Set to \code{FALSE} to skip this and return only the
#'   fields already in that response.
#'
#' @param max_number Numeric. Maximum number of matching names requested from the web
#'   service in a single call. Defaults to \code{1000}, generous for any genus.
#'
#' @param verbose Logical. If \code{TRUE} (default), prints progress messages.
#'
#' @param save Logical. If \code{TRUE} (default), the result is saved to disk as an
#'   \code{.xlsx} spreadsheet.
#'
#' @param dir Character. Directory where the spreadsheet is saved when \code{save = TRUE}.
#'   Defaults to \code{"spfungorum_records"}.
#'
#' @param filename Character. Name of the \code{.xlsx} file (without extension) to save.
#'   Defaults to \code{"spfungorum_records_<taxon>"}.
#'
#' @param html_report Logical. If \code{TRUE} (default), also writes a self-contained
#'   HTML report (\code{<dir>/<filename>.html}) summarizing the results: KPI counts and
#'   the full record table as a sortable, filterable \pkg{DT} widget with buttons to copy
#'   or download it as CSV/Excel. Requires the \pkg{rmarkdown}, \pkg{DT}, and
#'   \pkg{htmltools} packages; if any is missing, the report is skipped with a message
#'   (the \code{.xlsx} spreadsheet is unaffected).
#'
#' @param open_report Logical. If \code{TRUE} (default in interactive sessions), opens
#'   the rendered HTML report in the default browser.
#'
#' @return A \code{data.frame}, one row per matching Index Fungorum name and, by default,
#'   with a Brazilian reported locality, with columns:
#' \describe{
#'   \item{Fungorum_Number}{Index Fungorum's numeric record identifier for the name.}
#'   \item{Taxon_name}{The binomial as registered in Index Fungorum.}
#'   \item{Authors}{Author citation.}
#'   \item{SpecificEpithet}{The bare specific epithet.}
#'   \item{Year}{Year of publication.}
#'   \item{Name_status}{Index Fungorum's nomenclatural status (e.g. \code{"Legitimate"}).}
#'   \item{Current_name}{Index Fungorum's currently accepted name for this entry, when
#'     different (i.e. when \code{Taxon_name} is itself a synonym). When
#'     \code{fetch_details = TRUE}, this is the freshly-scraped value from the name's own
#'     page rather than the bulk search response's own (occasionally stale) field.}
#'   \item{Fungorum_Location, Fungorum_Host}{Type specimen locality and host, as reported
#'     in Index Fungorum's own record (may be \code{NA} - not every record has these).}
#'   \item{Volume, Part, Page}{The publication's volume, part/issue, and page.}
#'   \item{PublishingAuthors}{Full author list of the publication the name appeared in,
#'     which may differ from \code{Authors} (the name's own author citation).}
#'   \item{Fungorum_URL}{Direct link to the name's Species Fungorum page.}
#'   \item{Fungorum_Journal}{The journal the name was published in (only when
#'     \code{fetch_details = TRUE}; not available from the bulk search response).}
#'   \item{Fungorum_Citation}{The full formatted citation as shown on the name's own
#'     Species Fungorum page (only when \code{fetch_details = TRUE}).}
#'   \item{Fungorum_Basionym}{The basionym or typification details, when present (only
#'     when \code{fetch_details = TRUE}).}
#'   \item{Fungorum_Current_Name_Citation}{The full citation of the currently accepted
#'     name (only when \code{fetch_details = TRUE}).}
#'   \item{Fungorum_LSID}{The Index Fungorum Life Science Identifier (only when
#'     \code{fetch_details = TRUE}).}
#'   \item{Fungorum_Brazil_Evidence}{Logical; \code{TRUE} when \code{Fungorum_Location}
#'     mentions Brazil (the country or any Brazilian state).}
#' }
#'
#' @seealso \code{\link{spfungorum_gap}}, \code{\link{mycobank_records}}
#'
#' @author
#' Domingos Cardoso
#'
#' @examples
#' \dontrun{
#' # All Index Fungorum information for a single species, only if it reports a
#' # Brazilian locality
#' sp <- spfungorum_records(taxon = "Phellinotus neoaridus")
#'
#' # Every Index Fungorum species-level name in genus Phellinotus with a
#' # Brazilian reported locality
#' genus_records <- spfungorum_records(taxon = "Phellinotus", rank = "genus")
#'
#' # Every matching name regardless of locality, for manual review
#' all_records <- spfungorum_records(taxon = "Phellinotus", rank = "genus",
#'                                       require_brazil_evidence = FALSE)
#' }
#'
#' @importFrom xml2 read_xml xml_find_all xml_find_first xml_text
#' @importFrom openxlsx write.xlsx
#'
#' @export

spfungorum_records <- function(taxon,
                                   rank = c("species", "genus"),
                                   require_brazil_evidence = TRUE,
                                   fetch_details = TRUE,
                                   max_number = 1000,
                                   verbose = TRUE,
                                   save = TRUE,
                                   dir = "spfungorum_records",
                                   filename = NULL,
                                   html_report = TRUE,
                                   open_report = interactive()) {

  if (missing(taxon) || is.null(taxon) || !is.character(taxon) || length(taxon) != 1) {
    stop("'taxon' must be a single character string (one species or genus name).",
         call. = FALSE)
  }
  rank <- match.arg(rank)
  taxon <- trimws(taxon)

  if (is.null(filename)) {
    filename <- paste0("spfungorum_records_", gsub("\\s+", "_", taxon))
  }

  if (verbose) {
    message("Querying Index Fungorum's web service for ", rank, " '", taxon, "'...")
  }

  fg <- .indexfungorum_name_search(taxon, rank = rank, max_number = max_number)

  if (verbose) {
    message("  Found ", nrow(fg), " matching name(s) in Index Fungorum for ",
           rank, " '", taxon, "'.")
  }

  n_any_locality <- nrow(fg)

  result <- data.frame(
    Fungorum_Number = fg$Fungorum_Number,
    Taxon_name = fg$Taxon_name,
    Authors = fg$Authors,
    SpecificEpithet = fg$SpecificEpithet,
    Year = fg$Year,
    Name_status = fg$Name_status,
    Current_name = fg$Current_name,
    Fungorum_Location = fg$Location,
    Fungorum_Host = fg$Host,
    Volume = fg$Volume,
    Part = fg$Part,
    Page = fg$Page,
    PublishingAuthors = fg$PublishingAuthors,
    Fungorum_URL = ifelse(is.na(fg$Fungorum_Number), NA_character_,
                          paste0("https://www.speciesfungorum.org/names/NamesRecord.asp?RecordID=",
                                fg$Fungorum_Number)),
    stringsAsFactors = FALSE
  )
  result$Fungorum_Brazil_Evidence <- vapply(result$Fungorum_Location,
                                            .location_mentions_brazil, logical(1))
  result <- result[order(result$Taxon_name), ]
  rownames(result) <- NULL

  if (require_brazil_evidence) {
    n_brazil <- sum(result$Fungorum_Brazil_Evidence, na.rm = TRUE)
    if (verbose) {
      message(sprintf("  %d of %d matching name(s) have Index Fungorum locality evidence for Brazil.",
                      n_brazil, nrow(result)))
    }
    result <- result[result$Fungorum_Brazil_Evidence, ]
    rownames(result) <- NULL
  }

  # --------------------------------------------------------------------
  # Optional per-record page visit for fields not in the bulk search
  # response (journal, full citation, basionym, current name's own
  # citation, LSID) - a fast, plain HTTP request per record, no headless
  # browser needed (unlike MycoBank).
  # --------------------------------------------------------------------
  if (fetch_details && nrow(result) > 0) {
    result$Fungorum_Journal <- NA_character_
    result$Fungorum_Citation <- NA_character_
    result$Fungorum_Basionym <- NA_character_
    result$Fungorum_Current_Name_Citation <- NA_character_
    result$Fungorum_LSID <- NA_character_

    for (i in seq_len(nrow(result))) {
      if (verbose) {
        message(sprintf("  Fetching Index Fungorum details %d/%d: %s",
                        i, nrow(result), result$Taxon_name[i]))
      }
      det <- .indexfungorum_record_page(result$Fungorum_Number[i])
      result$Fungorum_Journal[i] <- det$journal
      result$Fungorum_Citation[i] <- det$citation
      result$Fungorum_Basionym[i] <- det$basionym_or_type
      result$Fungorum_Current_Name_Citation[i] <- det$current_name_citation
      result$Fungorum_LSID[i] <- det$lsid
      # The freshly-scraped current name (from this record's own page) takes
      # precedence over the bulk NameSearch response's Current_name, which is
      # a cached value on the original name's row that can go stale relative
      # to the current name's own record.
      if (!is.na(det$current_name)) result$Current_name[i] <- det$current_name
    }
  }

  if (verbose) {
    message(sprintf("\n\u2713 %d Index Fungorum record(s) retrieved for %s '%s'",
                    nrow(result), rank, taxon))
  }

  if (save && nrow(result) > 0) {
    dir <- .arg_check_dir(dir)
    .save_xlsx(result, verbose = verbose, filename = filename, dir = dir)
  }

  if (html_report && nrow(result) > 0) {
    brazil_filtered <- require_brazil_evidence

    report_data <- list(
      taxon = taxon,
      rank = rank,
      n_fungorum = n_any_locality,
      n_any_locality = n_any_locality,
      n_records = nrow(result),
      brazil_filtered = brazil_filtered,
      result = result
    )

    dir <- .arg_check_dir(dir)
    .funga_render_report(template = "spfungorum_records_report.Rmd",
                         data_list = report_data,
                         taxon = taxon,
                         dir = dir,
                         filename = filename,
                         verbose = verbose,
                         open_report = open_report)
  }

  return(result)
}
