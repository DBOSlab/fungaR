#' Find fungal species with Brazilian type specimens in GBIF that are missing from FFB
#'
#' @description
#' Given a fungal genus, finds the species-level names whose type specimens -
#' holotypes, isotypes, lectotypes, syntypes, paratypes, and so on - were collected in
#' Brazil according to the \href{https://www.gbif.org}{Global Biodiversity Information
#' Facility} (GBIF), but that are not currently registered in the Flora e Funga do
#' Brasil (FFB). A type specimen collected in Brazil is direct evidence that the taxon
#' occurs in the country, so every name returned is a candidate for inclusion in FFB.
#'
#' @details
#' GBIF aggregates the type specimens digitised by herbaria and fungaria worldwide (e.g.
#' RB, K, NY, URM). The search runs in three steps, each covering the whole genus at
#' once rather than one request per species, so even large genera return in seconds:
#' \enumerate{
#'   \item a single faceted occurrence search lists every name in the genus with type
#'     specimens collected in Brazil, and how many;
#'   \item those names are resolved against the GBIF taxonomic backbone, which also
#'     gives each name's accepted species;
#'   \item for the names missing from FFB only, the type specimens themselves are
#'     retrieved (many names per request) to summarise their type status, herbarium/
#'     fungarium, catalogue number, and state.
#' }
#'
#' Type specimens are filed in GBIF under their current identification, which for a
#' type is usually the typified name itself or, when that name is now a synonym, its
#' accepted name. Only species-level names are reported.
#'
#' A candidate counts as "already in FFB" if either its GBIF name or its GBIF
#' \emph{accepted} name matches any FFB name (accepted or synonym, anywhere in the
#' checklist, since GBIF and FFB can disagree on which genus a species belongs to).
#' Names are compared ignoring case, diacritics, hyphens, and the genitive
#' \emph{-ii}/\emph{-i} ending, so orthographic variants are not reported as missing.
#'
#' Only Fungi are searched: the genus is looked up in the GBIF Fungi kingdom, so a
#' genus name shared with a plant, alga, or animal never resolves to the wrong group.
#'
#' The FFB side of the comparison uses the same locally cached FFB dataset as
#' \code{\link{funga_download}} and \code{\link{funga_parse}} (in the
#' \code{"funga_download"} folder), downloading it only when needed.
#'
#' @usage
#' gbif_gap(
#'   taxon,
#'   type_status = NULL,
#'   version = "latest",
#'   verbose = TRUE,
#'   save = TRUE,
#'   dir = "gbif_gap",
#'   filename = NULL,
#'   html_report = TRUE,
#'   open_report = interactive()
#' )
#'
#' @param taxon Character. A single fungal genus name (e.g. \code{"Trichoderma"},
#'   \code{"Phellinotus"}).
#'
#' @param type_status Character or \code{NULL}. GBIF type statuses counted as
#'   evidence (GBIF's current vocabulary, e.g. \code{"Holotype"}, \code{"Isotype"}).
#'   \code{NULL} (default) uses all primary and secondary type categories: Type,
#'   Holotype, Isotype, Lectotype, Isolectotype, Syntype, Isosyntype, Neotype,
#'   Isoneotype, Epitype, Isoepitype, Paratype, Isoparatype, Paralectotype,
#'   OriginalMaterial, and TypeSeries. Use e.g. \code{c("Holotype", "Isotype")} for a
#'   stricter search.
#'
#' @param version Character. FFB dataset version to compare against. Defaults to
#'   \code{"latest"}. Passed to \code{\link{funga_download}}.
#'
#' @param verbose Logical. If \code{TRUE} (default), prints progress messages.
#'
#' @param save Logical. If \code{TRUE} (default), the result is saved to disk as an
#'   \code{.xlsx} spreadsheet.
#'
#' @param dir Character. Directory where the spreadsheet is saved when \code{save = TRUE}.
#'   Defaults to \code{"gbif_gap"}.
#'
#' @param filename Character. Name of the \code{.xlsx} file (without extension) to save.
#'   Defaults to \code{"gbif_gap_<taxon>"}.
#'
#' @param html_report Logical. If \code{TRUE} (default), also writes a self-contained
#'   HTML report (\code{<dir>/<filename>.html}) summarizing the results: KPI counts and
#'   the full candidate table as a sortable, filterable \pkg{DT} widget with buttons to
#'   copy or download it as CSV/Excel. Requires the \pkg{rmarkdown}, \pkg{DT}, and
#'   \pkg{htmltools} packages; if any is missing, the report is skipped with a message
#'   (the \code{.xlsx} spreadsheet is unaffected).
#'
#' @param open_report Logical. If \code{TRUE} (default in interactive sessions), opens
#'   the HTML report in the browser once rendered.
#'
#' @return A \code{data.frame}, one row per species-level GBIF name in the requested
#'   genus that has type specimens collected in Brazil and is absent from the current
#'   FFB checklist, with columns:
#' \describe{
#'   \item{GBIF_Key}{The name's GBIF backbone key.}
#'   \item{Taxon_name}{The name as registered in GBIF.}
#'   \item{Authors}{Author citation.}
#'   \item{Taxonomic_status}{GBIF's taxonomic status (e.g. \code{"ACCEPTED"},
#'     \code{"SYNONYM"}, \code{"DOUBTFUL"}).}
#'   \item{Accepted_name, Accepted_full_name}{GBIF's accepted species for this name,
#'     without and with authors (the name itself when it is accepted).}
#'   \item{N_type_specimens}{Number of type specimens collected in Brazil in GBIF.}
#'   \item{Type_statuses}{The type statuses among them (e.g. \code{"Holotype; Isotype"}).}
#'   \item{Type_specimens}{Up to ten specimens as herbarium/fungarium and catalogue
#'     number, with type status (e.g. \code{"URM 80362 (Holotype)"}).}
#'   \item{Specimen_names}{The names the type specimens are filed under in GBIF -
#'     usually the typified name, which for a synonym differs from
#'     \code{Taxon_name} (e.g. the types of a species' basionym).}
#'   \item{Collectors, Years, States}{Collectors, collection years, and Brazilian
#'     states of those specimens.}
#'   \item{GBIF_Published_in}{Protologue citation, when GBIF has it.}
#'   \item{GBIF_URL}{Link to the name's GBIF page.}
#'   \item{In_FFB}{Always \code{FALSE} (kept for clarity when combining with other tables).}
#' }
#'
#' @seealso \code{\link{mycobank_gap}}, \code{\link{spfungorum_gap}},
#'   \code{\link{funga_download}}
#'
#' @author
#' Domingos Cardoso
#'
#' @examples
#' \dontrun{
#' # Species of Trichoderma with Brazilian type specimens that are missing from FFB
#' gap <- gbif_gap(taxon = "Trichoderma")
#'
#' # Stricter evidence: holotypes and isotypes only
#' gap_strict <- gbif_gap(taxon = "Trichoderma", type_status = c("Holotype", "Isotype"))
#' }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom openxlsx write.xlsx
#'
#' @export

gbif_gap <- function(taxon,
                     type_status = NULL,
                     version = "latest",
                     verbose = TRUE,
                     save = TRUE,
                     dir = "gbif_gap",
                     filename = NULL,
                     html_report = TRUE,
                     open_report = interactive()) {

  if (missing(taxon) || is.null(taxon) || !is.character(taxon) || length(taxon) != 1) {
    stop("'taxon' must be a single character string (one genus name).",
         call. = FALSE)
  }
  taxon <- trimws(taxon)
  if (is.null(type_status)) type_status <- .gbif_type_status

  if (is.null(filename)) {
    filename <- paste0("gbif_gap_", gsub("\\s+", "_", taxon))
  }

  # ------------------------------------------------------------------------
  # 1. GBIF genus (fail fast, before the slower FFB step)
  # ------------------------------------------------------------------------
  genus <- .gbif_genus_key(taxon)
  if (is.null(genus)) {
    stop("Genus '", taxon, "' was not found in the GBIF backbone among Fungi. ",
         "Check the spelling.", call. = FALSE)
  }
  if (verbose) message("GBIF genus: ", genus$name, " (", genus$kingdom, ")")

  # ------------------------------------------------------------------------
  # 2. FFB side: names already registered (genus species + all of FFB)
  # ------------------------------------------------------------------------
  if (verbose) message("Retrieving the current FFB checklist for '", taxon, "'...")

  ffb <- tryCatch(.ffb_names(taxon, version = version), error = function(e) {
    if (verbose) message("  Could not read the FFB checklist: ", conditionMessage(e))
    list(genus = character(0), all = character(0))
  })
  if (verbose && length(ffb$genus) == 0) {
    message("  No species of '", taxon, "' found in the current FFB checklist ",
            "(treating FFB's known species list as empty).")
  }

  # ------------------------------------------------------------------------
  # 3. GBIF side: names with Brazilian type specimens, resolved to names
  # ------------------------------------------------------------------------
  if (verbose) message("Searching GBIF for type specimens collected in Brazil...")

  counts <- .gbif_brazil_type_counts(genus$key, type_status)
  if (is.null(counts)) {
    stop("The GBIF occurrence search failed; please try again later.", call. = FALSE)
  }
  names_df <- .gbif_genus_names(genus$key)

  # Keys outside the genus listing (rare) are looked up one by one
  extra <- setdiff(counts$GBIF_Key, names_df$GBIF_Key)
  for (k in extra) {
    r <- .get_json(paste0("https://api.gbif.org/v1/species/", k))
    if (is.null(r)) next
    val <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
    names_df <- rbind(names_df, data.frame(
      GBIF_Key = val(r$key), Taxon_name = val(r$canonicalName), Authors = val(r$authorship),
      Rank = val(r$rank), Taxonomic_status = val(r$taxonomicStatus),
      Accepted_name = val(r$species),
      Accepted_full_name = if (is.null(r$accepted)) val(r$scientificName) else val(r$accepted),
      Genus = val(r$genus), GBIF_Published_in = val(r$publishedIn), stringsAsFactors = FALSE))
  }

  typed <- merge(counts, names_df, by = "GBIF_Key")
  typed <- typed[typed$Rank %in% "SPECIES" & !is.na(typed$Taxon_name), ]
  typed$Accepted_name[is.na(typed$Accepted_name)] <- typed$Taxon_name[is.na(typed$Accepted_name)]

  if (verbose) {
    message("  ", nrow(typed), " species-level name(s) in '", taxon,
            "' have type specimens collected in Brazil.")
  }

  # ------------------------------------------------------------------------
  # 4. Names absent from the FFB checklist
  # ------------------------------------------------------------------------
  # A candidate counts as "already in FFB" if EITHER its GBIF name OR its
  # accepted name matches any FFB name (the accepted name may sit in a
  # different genus), compared by .name_key() to ignore orthographic
  # variants.
  ffb_keys <- .name_key(ffb$all)
  already_in_ffb <- .name_key(typed$Taxon_name) %in% ffb_keys |
    .name_key(typed$Accepted_name) %in% ffb_keys
  missing <- typed[!already_in_ffb, ]
  missing <- missing[order(missing$Taxon_name), ]
  rownames(missing) <- NULL

  if (verbose) {
    message("  ", nrow(missing), " of those are not currently in FFB.")
  }

  # ------------------------------------------------------------------------
  # 5. Summarise the Brazilian type specimens of the missing names only
  # ------------------------------------------------------------------------
  if (nrow(missing) > 0 && verbose) message("Retrieving their type specimens...")
  specimens <- if (nrow(missing) > 0) {
    .gbif_type_specimens(missing$GBIF_Key, type_status)
  } else {
    .gbif_type_specimens(character(0))
  }

  # Assign each specimen to the requested name it was retrieved for: its own
  # key, or else - for types filed under a synonym - its accepted or
  # species key
  owner <- ifelse(specimens$GBIF_Key %in% missing$GBIF_Key, specimens$GBIF_Key,
                  ifelse(specimens$Accepted_key %in% missing$GBIF_Key, specimens$Accepted_key,
                         specimens$Species_key))
  specimens$Owner <- owner

  collapse_unique <- function(x, max = Inf) {
    x <- unique(x[!is.na(x) & nzchar(x)])
    if (length(x) == 0) return(NA_character_)
    more <- if (length(x) > max) paste0("; ... (", length(x) - max, " more)") else ""
    paste0(paste(utils::head(x, max), collapse = "; "), more)
  }
  summarise <- function(key, field) {
    sp <- specimens[specimens$Owner %in% key, ]
    if (nrow(sp) == 0) return(NA_character_)
    switch(field,
           statuses = collapse_unique(unlist(strsplit(sp$Type_status, ",\\s*"))),
           specimens = collapse_unique(trimws(paste0(
             ifelse(is.na(sp$Institution), "", sp$Institution), " ",
             ifelse(is.na(sp$Catalog_number), "", sp$Catalog_number),
             " (", sp$Type_status, ")")), max = 10),
           names = collapse_unique(sp$Specimen_name),
           collectors = collapse_unique(sp$Collector, max = 5),
           years = if (all(is.na(sp$Year))) NA_character_ else {
             y <- range(as.integer(sp$Year), na.rm = TRUE)
             if (y[1] == y[2]) as.character(y[1]) else paste(y, collapse = "-")
           },
           states = collapse_unique(sp$State))
  }
  per_name <- function(field) {
    vapply(missing$GBIF_Key, summarise, character(1), field = field, USE.NAMES = FALSE)
  }

  result <- data.frame(
    GBIF_Key = missing$GBIF_Key,
    Taxon_name = missing$Taxon_name,
    Authors = missing$Authors,
    Taxonomic_status = missing$Taxonomic_status,
    Accepted_name = missing$Accepted_name,
    Accepted_full_name = missing$Accepted_full_name,
    N_type_specimens = missing$N_type_specimens,
    Type_statuses = per_name("statuses"),
    Type_specimens = per_name("specimens"),
    Specimen_names = per_name("names"),
    Collectors = per_name("collectors"),
    Years = per_name("years"),
    States = per_name("states"),
    GBIF_Published_in = missing$GBIF_Published_in,
    # sprintf(), unlike paste0(), returns nothing for zero missing names
    GBIF_URL = sprintf("https://www.gbif.org/species/%s", missing$GBIF_Key),
    In_FFB = logical(nrow(missing)),
    stringsAsFactors = FALSE
  )

  if (verbose) {
    message(sprintf("\n\u2713 %d species with Brazilian type specimens found in GBIF but missing from FFB",
                    nrow(result)))
  }

  if (save && nrow(result) > 0) {
    dir <- .arg_check_dir(dir)
    .save_xlsx(result, verbose = verbose, filename = filename, dir = dir)
  }

  if (html_report && nrow(result) > 0) {
    report_data <- list(
      taxon = taxon,
      rank = "genus",
      source = paste0("GBIF type specimens collected in Brazil (", genus$name, ", ",
                      genus$kingdom, ")"),
      n_typed = nrow(typed),
      n_in_ffb = length(ffb$genus),
      n_missing = nrow(result),
      n_specimens = sum(result$N_type_specimens),
      result = result
    )

    dir <- .arg_check_dir(dir)
    .funga_render_report(template = "gbif_gap_report.Rmd",
                         data_list = report_data,
                         taxon = taxon,
                         dir = dir,
                         filename = filename,
                         verbose = verbose,
                         open_report = open_report)
  }

  return(result)
}
