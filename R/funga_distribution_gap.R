#' Find candidate new Brazilian state records for a fungal taxon
#'
#' @description
#' Given a fungal genus or species, checks \href{https://www.gbif.org}{GBIF},
#' \href{https://specieslink.net}{speciesLink}, and (optionally) the
#' \href{https://ipt.jbrj.gov.br/reflora/}{Reflora Virtual Herbarium} for specimen/occurrence
#' evidence of the taxon in Brazilian states that are \emph{not} currently listed in its
#' official Flora e Funga do Brasil (FFB) distribution. This flags candidate new state
#' records for a taxonomist to manually verify - e.g. a species listed by FFB only for
#' Bahia, but with specimens indexed from Pernambuco.
#'
#' @details
#' FFB's own officially-listed states are retrieved the same way \code{\link{funga_records}}
#' does internally, from the parsed FFB distribution table. Occurrence evidence beyond FFB
#' comes from up to three independent sources (see \code{sources}): GBIF's public REST API
#' (no key required, and the most reliable of the three), speciesLink's public web service
#' (best-effort - see \code{\link{funga_mycobank_gap}}'s Note on its known SSL/endpoint
#' issues), and the \pkg{refloraR} package's \code{\link[refloraR]{reflora_records}} for
#' REFLORA specimen data (used only if the \pkg{refloraR} package is installed).
#'
#' @usage
#' funga_distribution_gap(
#'   taxon,
#'   state = NULL,
#'   sources = c("gbif", "speciesLink", "reflora"),
#'   version = "latest",
#'   verbose = TRUE,
#'   save = TRUE,
#'   dir = "funga_distribution_gap",
#'   filename = NULL,
#'   html_report = TRUE,
#'   open_report = interactive()
#' )
#'
#' @param taxon Character. A single fungal genus or species name (e.g.
#'   \code{"Trichoderma harzianum"} or \code{"Trichoderma"}).
#'
#' @param state Character vector. Optional subset of Brazilian states (full name or
#'   acronym) to restrict the check to. \code{NULL} (default) checks all states with any
#'   evidence from the requested \code{sources}.
#'
#' @param sources Character vector. Which occurrence sources to query: any of
#'   \code{"gbif"}, \code{"speciesLink"}, \code{"reflora"}. Defaults to all three.
#'
#' @param version Character. FFB dataset version to compare against. Defaults to
#'   \code{"latest"}. Passed to \code{\link{funga_records}}.
#'
#' @param verbose Logical. If \code{TRUE} (default), prints progress messages.
#'
#' @param save Logical. If \code{TRUE} (default), the result is saved to disk as an
#'   \code{.xlsx} spreadsheet.
#'
#' @param dir Character. Directory where the spreadsheet is saved when \code{save = TRUE}.
#'   Defaults to \code{"funga_distribution_gap"}.
#'
#' @param filename Character. Name of the \code{.xlsx} file (without extension) to save.
#'   Defaults to \code{"funga_distribution_gap_<taxon>"}.
#'
#' @param html_report Logical. If \code{TRUE} (default), also writes a self-contained
#'   HTML report (\code{<dir>/<filename>.html}) summarizing the results: KPI counts, an
#'   evidence-by-source breakdown, and the full state-by-state table as a sortable,
#'   filterable \pkg{DT} widget with buttons to copy or download it as CSV/Excel.
#'   Requires the \pkg{rmarkdown}, \pkg{DT}, and \pkg{htmltools} packages; if any is
#'   missing, the report is skipped with a message (the \code{.xlsx} spreadsheet is
#'   unaffected).
#'
#' @param open_report Logical. If \code{TRUE} (default in interactive sessions), opens
#'   the rendered HTML report in the default browser.
#'
#' @return A \code{data.frame}, one row per Brazilian state with any occurrence evidence
#'   for \code{taxon}, with columns:
#' \describe{
#'   \item{Taxon}{The queried taxon name.}
#'   \item{State}{Brazilian state name.}
#'   \item{In_FFB_distribution}{Logical; whether FFB already lists this state for the taxon.}
#'   \item{GBIF_records}{Number of Brazil GBIF occurrence records in this state (when
#'     \code{"gbif" \%in\% sources}).}
#'   \item{speciesLink_evidence}{Logical; whether speciesLink reported this state
#'     (best-effort, see Details).}
#'   \item{REFLORA_records}{Number of REFLORA specimen records in this state (when
#'     \code{"reflora" \%in\% sources} and \pkg{refloraR} is installed).}
#'   \item{New_state_record_candidate}{\code{TRUE} when the state has evidence from at
#'     least one external source but is \emph{not} in \code{In_FFB_distribution} - i.e. a
#'     candidate worth manual taxonomic review.}
#' }
#'
#' @seealso \code{\link{funga_mycobank_gap}}, \code{\link{funga_records}}
#'
#' @author
#' Domingos Cardoso
#'
#' @examples
#' \dontrun{
#' gap <- funga_distribution_gap(taxon = "Trichoderma harzianum")
#'
#' # Restrict the check to two states, using only GBIF
#' gap_ba_pe <- funga_distribution_gap(taxon = "Trichoderma harzianum",
#'                                     state = c("Bahia", "Pernambuco"),
#'                                     sources = "gbif")
#' }
#'
#' @importFrom openxlsx write.xlsx
#'
#' @export

funga_distribution_gap <- function(taxon,
                                   state = NULL,
                                   sources = c("gbif", "speciesLink", "reflora"),
                                   version = "latest",
                                   verbose = TRUE,
                                   save = TRUE,
                                   dir = "funga_distribution_gap",
                                   filename = NULL,
                                   html_report = TRUE,
                                   open_report = interactive()) {

  if (missing(taxon) || is.null(taxon) || !is.character(taxon) || length(taxon) != 1) {
    stop("'taxon' must be a single character string (one genus or species name).",
         call. = FALSE)
  }
  sources <- match.arg(sources, c("gbif", "speciesLink", "reflora"), several.ok = TRUE)
  taxon <- trimws(taxon)

  if (is.null(filename)) {
    filename <- paste0("funga_distribution_gap_", gsub("\\s+", "_", taxon))
  }

  # ------------------------------------------------------------------------
  # 1. FFB's officially-listed states for this taxon
  # ------------------------------------------------------------------------
  if (verbose) message("Retrieving FFB's officially listed states for '", taxon, "'...")

  ffb <- .funga_prepare_records(version = version, verbose = FALSE, rm_funga_database = FALSE)
  taxon_ids <- ffb$taxon_df$id[ffb$taxon_df$taxonName %in% taxon]

  ffb_states <- character(0)
  if (length(taxon_ids) > 0) {
    abbrev <- gsub("^BR-", "", ffb$distribution_df$locationID[ffb$distribution_df$id %in% taxon_ids])
    abbrev <- abbrev[!is.na(abbrev) & nzchar(abbrev)]
    if (length(abbrev) > 0) ffb_states <- unique(.arg_check_state(abbrev, return_abbrev = FALSE))
  } else if (verbose) {
    message("  '", taxon, "' was not found in the current FFB checklist ",
           "(treating FFB's officially listed states as empty).")
  }

  # ------------------------------------------------------------------------
  # 2. External occurrence evidence, one source at a time
  # ------------------------------------------------------------------------
  gbif_states_df <- data.frame(State = character(0), GBIF_records = integer(0))
  if ("gbif" %in% sources) {
    if (verbose) message("Querying GBIF for Brazil occurrence records...")
    gbif <- .gbif_brazil_occurrence(taxon)
    if (nrow(gbif$state_counts) > 0) {
      gbif_states_df <- data.frame(State = .arg_check_state(gbif$state_counts$state),
                                   GBIF_records = gbif$state_counts$count,
                                   stringsAsFactors = FALSE)
    }
  }

  splink_states <- character(0)
  if ("speciesLink" %in% sources) {
    if (verbose) message("Querying speciesLink for Brazil occurrence records (best-effort)...")
    splink <- .splink_brazil_occurrence(taxon)
    if (!is.na(splink$states) && nzchar(splink$states)) {
      splink_states <- .arg_check_state(trimws(strsplit(splink$states, ";")[[1]]))
    }
  }

  reflora_states_df <- data.frame(State = character(0), REFLORA_records = integer(0))
  if ("reflora" %in% sources) {
    if (!requireNamespace("refloraR", quietly = TRUE)) {
      if (verbose) {
        message("  Package 'refloraR' is not installed; skipping REFLORA evidence. ",
               "Install it with install.packages('refloraR') to include this source.")
      }
    } else {
      if (verbose) message("Querying REFLORA specimen records (via refloraR)...")
      reflora_result <- tryCatch({
        refloraR::reflora_records(taxon = taxon, verbose = FALSE, save = FALSE)
      }, error = function(e) {
        if (verbose) message("  REFLORA query failed: ", conditionMessage(e))
        NULL
      })
      if (!is.null(reflora_result) && nrow(reflora_result) > 0 &&
          "stateProvince" %in% names(reflora_result)) {
        tab <- table(.arg_check_state(stats::na.omit(reflora_result$stateProvince)))
        reflora_states_df <- data.frame(State = names(tab), REFLORA_records = as.integer(tab),
                                        stringsAsFactors = FALSE)
      }
    }
  }

  # ------------------------------------------------------------------------
  # 3. Assemble one row per state with ANY evidence (FFB or external)
  # ------------------------------------------------------------------------
  all_states <- unique(c(ffb_states, gbif_states_df$State, splink_states, reflora_states_df$State))

  if (!is.null(state)) {
    requested <- .arg_check_state(state)
    all_states <- intersect(all_states, requested)
  }

  if (length(all_states) == 0) {
    if (verbose) message("\nNo occurrence evidence found for '", taxon, "' from the requested sources.")
    result <- data.frame(Taxon = character(0), State = character(0),
                         In_FFB_distribution = logical(0),
                         New_state_record_candidate = logical(0),
                         stringsAsFactors = FALSE)
    return(result)
  }

  result <- data.frame(
    Taxon = taxon,
    State = sort(all_states),
    stringsAsFactors = FALSE
  )
  result$In_FFB_distribution <- result$State %in% ffb_states

  if ("gbif" %in% sources) {
    result$GBIF_records <- gbif_states_df$GBIF_records[match(result$State, gbif_states_df$State)]
  }
  if ("speciesLink" %in% sources) {
    result$speciesLink_evidence <- result$State %in% splink_states
  }
  if ("reflora" %in% sources) {
    result$REFLORA_records <- reflora_states_df$REFLORA_records[match(result$State, reflora_states_df$State)]
  }

  result$New_state_record_candidate <- !result$In_FFB_distribution

  rownames(result) <- NULL

  if (verbose) {
    n_new <- sum(result$New_state_record_candidate)
    message(sprintf("\n\u2713 %d state(s) with occurrence evidence for '%s', %d not yet in FFB's distribution",
                    nrow(result), taxon, n_new))
  }

  if (save && nrow(result) > 0) {
    dir <- .arg_check_dir(dir)
    .save_xlsx(result, verbose = verbose, filename = filename, dir = dir)
  }

  if (html_report && nrow(result) > 0) {
    source_summary <- data.frame(
      source = c("FFB (official)", "GBIF", "speciesLink", "REFLORA"),
      n_states = c(
        length(ffb_states),
        if ("gbif" %in% sources) nrow(gbif_states_df) else NA_integer_,
        if ("speciesLink" %in% sources) length(splink_states) else NA_integer_,
        if ("reflora" %in% sources) nrow(reflora_states_df) else NA_integer_
      ),
      stringsAsFactors = FALSE
    )
    source_summary <- source_summary[!is.na(source_summary$n_states), ]

    report_data <- list(
      taxon = taxon,
      n_in_ffb = length(ffb_states),
      n_states_total = nrow(result),
      n_new_candidates = sum(result$New_state_record_candidate),
      source_summary = source_summary,
      result = result
    )

    dir <- .arg_check_dir(dir)
    .funga_render_report(template = "funga_distribution_gap_report.Rmd",
                         data_list = report_data,
                         taxon = taxon,
                         dir = dir,
                         filename = filename,
                         verbose = verbose,
                         open_report = open_report)
  }

  return(result)
}
