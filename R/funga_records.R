#' Retrieve taxon records from the Flora e Funga do Brasil database
#'
#' @description
#' Retrieves and filters fungal taxon records from the locally parsed
#' \href{https://floradobrasil.jbrj.gov.br/consulta/}{Flora e Funga do Brasil (FFB)}
#' taxonomic database, hosted by the \href{https://www.gov.br/jbrj/pt-br}{Rio de Janeiro
#' Botanical Garden}. Unlike \code{\link{funga_search}}, which resolves a list of
#' species names you already have against the FFB database, \code{funga_records()}
#' browses and filters the FFB checklist itself by taxonomic, geographic, and
#' trait-based criteria; no input name list required.
#'
#' @details
#' The function downloads (if needed) and parses the FFB dataset exactly like
#' \code{\link{funga_search}}, then applies the requested filters:
#' \itemize{
#'   \item \code{taxon} is matched against the \code{family}, \code{genus}, and
#'         \code{taxonName} columns of the FFB taxon table (family names ending
#'         in \code{"aceae"}, single-word genus names, and multi-word species
#'         names are detected automatically).
#'   \item \code{taxonRank} and \code{taxonomicStatus} are matched directly
#'         against the taxon table.
#'   \item \code{state}, \code{phytogeographicDomain}, and \code{endemism} are
#'         matched against the FFB distribution table (\code{distribution.txt}).
#'   \item \code{lifeForm}, \code{habitat}, and \code{vegetationType} are matched
#'         against the FFB species profile table (\code{speciesprofile.txt}).
#' }
#' All filters are combined with a logical AND. The result always has one row
#' per matching taxon (geographic/trait filters restrict \emph{which} taxa are
#' returned, they do not multiply rows).
#'
#' @usage
#' funga_records(
#'   taxon = NULL,
#'   taxonRank = NULL,
#'   taxonomicStatus = NULL,
#'   state = NULL,
#'   phytogeographicDomain = NULL,
#'   endemism = NULL,
#'   lifeForm = NULL,
#'   habitat = NULL,
#'   vegetationType = NULL,
#'   version = "latest",
#'   rm_funga_database = FALSE,
#'   verbose = TRUE,
#'   save = FALSE,
#'   dir = "funga_records",
#'   filename = "funga_records_search"
#' )
#'
#' @param taxon Character vector. One or more family, genus, or species names
#'   to filter by (e.g. \code{c("Hymenochaetaceae", "Trichoderma", "Xylaria
#'   hypoxylon")}). \code{NULL} (default) does not filter by taxon.
#'
#' @param taxonRank Character vector. Taxonomic rank(s) to keep (e.g.
#'   \code{"ESPECIE"}, \code{"VARIEDADE"}, \code{"SUBESPECIE"}, \code{"FORMA"},
#'   \code{"GENERO"}, \code{"ORDEM"}, \code{"DIVISAO"} - fungi are classified
#'   in FFB by Division/phylum rather than Class). \code{NULL} (default) keeps
#'   all ranks.
#'
#' @param taxonomicStatus Character vector. FFB taxonomic status to keep:
#'   \code{"NOME_ACEITO"} (accepted), \code{"SINONIMO"} (synonym), or
#'   \code{"NOME_DUVIDOSO"} (doubtful). \code{NULL} (default) keeps all statuses.
#'
#' @param state Character vector. Brazilian state(s) - full name or acronym,
#'   diacritics-insensitive (e.g. \code{"Bahia"} or \code{"BA"}) - to filter by
#'   occurrence, based on the FFB distribution table. \code{NULL} (default)
#'   does not filter by state.
#'
#' @param phytogeographicDomain Character vector. Phytogeographic domain(s) to
#'   filter by (e.g. \code{"Caatinga"}, \code{"Mata Atlantica"}). \code{NULL}
#'   (default) does not filter by domain.
#'
#' @param endemism Logical. If \code{TRUE}, keeps only taxa flagged as
#'   Brazilian endemics; if \code{FALSE}, keeps only non-endemics. \code{NULL}
#'   (default) does not filter by endemism.
#'
#' @param lifeForm,habitat,vegetationType Character vectors, based on the FFB
#'   species profile table. \code{NULL} (default) does not filter.
#'   \itemize{
#'     \item \code{lifeForm}: nutritional/ecological mode, e.g.
#'       \code{"Saprobio"}, \code{"Parasita"}, \code{"Liquenizado"},
#'       \code{"Micorrizico"}, \code{"Endofitico"}, \code{"Entomogeno"}.
#'     \item \code{habitat}: for fungi this field records the
#'       \strong{substrate/host} the fungus was collected on rather than a
#'       plant growth habitat, e.g. \code{"Planta viva - raiz"},
#'       \code{"Planta viva - folha"}, \code{"Planta viva - cortex do caule"},
#'       \code{"Tronco em decomposicao"}, \code{"Folhedo"}, \code{"Solo"},
#'       \code{"Esterco ou Fezes"}, \code{"Outro fungo"}, \code{"Animal morto"}.
#'       Use this argument to browse the checklist by host/substrate type (FFB
#'       does not currently publish a separate named host-taxon field).
#'     \item \code{vegetationType}: vegetation type where the record was made,
#'       e.g. \code{"Floresta Ombrofila (= Floresta Pluvial)"},
#'       \code{"Cerrado (lato sensu)"}, \code{"Manguezal"}.
#'   }
#'
#' @param version Character. FFB dataset version to use. Defaults to
#'   \code{"latest"}. Passed to \code{\link{funga_download}} and
#'   \code{\link{funga_parse}}.
#'
#' @param rm_funga_database Logical. If \code{TRUE}, the downloaded FFB database
#'   folder (\code{"funga_download"}) is deleted after the search is complete.
#'   If \code{FALSE} (default), the database is kept on disk and reused by
#'   subsequent calls, which significantly speeds up repeated queries.
#'
#' @param verbose Logical. If \code{TRUE} (default), prints informative
#'   progress messages during download and parsing. If \code{FALSE}, runs
#'   quietly.
#'
#' @param save Logical. If \code{TRUE}, the filtered records are saved to disk
#'   as a CSV file. Default is \code{FALSE}.
#'
#' @param dir Character. Directory where the CSV file will be saved when
#'   \code{save = TRUE}. Defaults to \code{"funga_records"}.
#'
#' @param filename Character. Name of the CSV file (without extension) to save
#'   when \code{save = TRUE}. Defaults to \code{"funga_records_search"}.
#'
#' @return A \code{data.frame} of FFB taxon records matching the requested
#'   filters, with one row per taxon. If \code{save = TRUE}, the result is also
#'   written to \code{<dir>/<filename>.csv}.
#'
#' @section Database caching behavior:
#' The FFB dataset is downloaded only once and cached locally in the
#' \code{"funga_download"} folder. On subsequent calls:
#' \itemize{
#'   \item If \code{rm_funga_database = FALSE} (default), the function checks if
#'     the cached version matches the requested \code{version}. If yes, it
#'     reuses the existing download; if not, it downloads the correct version.
#'   \item If \code{rm_funga_database = TRUE}, the database is deleted after
#'     each call, forcing a fresh download on every call (not recommended for
#'     repeated use).
#' }
#'
#' @seealso
#' \code{\link{funga_search}} to resolve a list of names you already have.
#' \code{\link{funga_download}} to manually download the DwC-A dataset.
#' \code{\link{funga_parse}} to manually parse the downloaded dataset.
#'
#' @author
#' Domingos Cardoso
#'
#' @examples
#' \dontrun{
#' # All accepted species in Xylariaceae (downloads + parses automatically, caches result)
#' xylariaceae <- funga_records(taxon = "Xylariaceae",
#'                              taxonomicStatus = "NOME_ACEITO")
#'
#' # Accepted species endemic to Bahia
#' bahia_endemics <- funga_records(state = "Bahia",
#'                                 endemism = TRUE,
#'                                 taxonomicStatus = "NOME_ACEITO")
#'
#' # Lichenized fungi recorded in the Caatinga
#' caatinga_lichens <- funga_records(phytogeographicDomain = "Caatinga",
#'                                   lifeForm = "Liquenizado")
#'
#' # Fungi recorded growing on living plant roots (habitat = substrate/host)
#' root_associates <- funga_records(habitat = "Planta viva - raiz")
#'
#' # Use a specific FFB version stored in a custom folder, and save the result
#' funga_records(taxon = "Trichoderma",
#'              version = "393.418",
#'              save = TRUE,
#'              dir = "funga_records",
#'              filename = "trichoderma_records")
#' }
#'
#' @importFrom dplyr filter
#' @importFrom stringi stri_trans_general
#'
#' @export

funga_records <- function(taxon = NULL,
                          taxonRank = NULL,
                          taxonomicStatus = NULL,
                          state = NULL,
                          phytogeographicDomain = NULL,
                          endemism = NULL,
                          lifeForm = NULL,
                          habitat = NULL,
                          vegetationType = NULL,
                          version = "latest",
                          rm_funga_database = FALSE,
                          verbose = TRUE,
                          save = FALSE,
                          dir = "funga_records",
                          filename = "funga_records_search") {

  ffb <- .funga_prepare_records(version = version,
                                verbose = verbose,
                                rm_funga_database = rm_funga_database)

  records <- ffb$taxon_df
  distribution_df <- ffb$distribution_df
  speciesprofile_df <- ffb$speciesprofile_df

  # Filter by taxon name (family/genus/species) -- reuses the existing
  # family/genus/species auto-detection logic (state handled separately below,
  # since FFB distribution data uses 'locationID', not 'stateProvince')
  if (!is.null(taxon)) {
    records <- .filter_occur_df(records, taxon = taxon, state = NULL, verbose = verbose)
  }

  # Filter by taxonRank
  if (!is.null(taxonRank)) {
    taxonRank <- toupper(trimws(taxonRank))
    records <- records[records$taxonRank %in% taxonRank, ]
  }

  # Filter by taxonomicStatus
  if (!is.null(taxonomicStatus)) {
    taxonomicStatus <- toupper(trimws(taxonomicStatus))
    records <- records[records$taxonomicStatus %in% taxonomicStatus, ]
  }

  # Filter by state, via the FFB distribution table
  if (!is.null(state)) {
    state_abbrev <- .arg_check_state(state, return_abbrev = TRUE)
    dist_ids <- distribution_df$id[gsub("^BR-", "", distribution_df$locationID) %in% state_abbrev]
    records <- records[records$id %in% dist_ids, ]
  }

  # Filter by phytogeographic domain, via the FFB distribution table
  if (!is.null(phytogeographicDomain)) {
    domain_norm <- tolower(trimws(phytogeographicDomain))
    dist_domain_norm <- tolower(trimws(distribution_df$phytogeographicDomain))
    dist_ids <- distribution_df$id[dist_domain_norm %in% domain_norm]
    records <- records[records$id %in% dist_ids, ]
  }

  # Filter by endemism, via the FFB distribution table
  if (!is.null(endemism)) {
    truthy <- c("true", "endemic", "endemica", "end\u00eamica", "sim")
    falsy  <- c("false", "not endemic", "nao endemica", "n\u00e3o end\u00eamica", "nao", "n\u00e3o")
    tokens <- if (isTRUE(endemism)) truthy else falsy
    dist_endemism_norm <- tolower(trimws(as.character(distribution_df$endemism)))
    dist_ids <- distribution_df$id[dist_endemism_norm %in% tokens]
    records <- records[records$id %in% dist_ids, ]
  }

  # Filter by lifeForm / habitat / vegetationType, via the FFB species profile table
  if (!is.null(lifeForm)) {
    lifeForm_norm <- tolower(trimws(lifeForm))
    sp_ids <- speciesprofile_df$id[tolower(trimws(speciesprofile_df$lifeForm)) %in% lifeForm_norm]
    records <- records[records$id %in% sp_ids, ]
  }
  if (!is.null(habitat)) {
    habitat_norm <- tolower(trimws(habitat))
    sp_ids <- speciesprofile_df$id[tolower(trimws(speciesprofile_df$habitat)) %in% habitat_norm]
    records <- records[records$id %in% sp_ids, ]
  }
  if (!is.null(vegetationType)) {
    vegetation_norm <- tolower(trimws(vegetationType))
    sp_ids <- speciesprofile_df$id[tolower(trimws(speciesprofile_df$vegetationType)) %in% vegetation_norm]
    records <- records[records$id %in% sp_ids, ]
  }

  # Append the distribution and species-profile information for each
  # returned taxon, concatenating multiple values (e.g. several states, or
  # several habitat/substrate records) with " | " so the result stays one
  # row per taxon instead of multiplying rows.
  records <- .funga_append_distribution_speciesprofile(records, distribution_df, speciesprofile_df)

  rownames(records) <- NULL

  if (verbose) {
    message(sprintf("\n\u2713 Returned %d taxon record(s) matching the requested filters", nrow(records)))
  }

  if (save) {
    dir <- .arg_check_dir(dir)
    .save_csv(records, verbose = verbose, filename = filename, dir = dir)
  }

  return(records)
}


#_______________________________________________________________________________
# Download (if needed) and parse the FFB dataset, returning the taxon,
# distribution, and species profile tables shared by funga_records().
.funga_prepare_records <- function(version, verbose, rm_funga_database) {
  funga_download(version = version, dir = "funga_download", verbose = verbose)
  dwca <- funga_parse(path = "funga_download", version = version, verbose = verbose)

  key <- names(dwca)[1L]
  taxon_df <- dwca[[key]][["data"]][["taxon.txt"]]
  distribution_df <- dwca[[key]][["data"]][["distribution.txt"]]
  speciesprofile_df <- dwca[[key]][["data"]][["speciesprofile.txt"]]

  if (is.null(taxon_df)) {
    stop("No 'taxon.txt' table found in the parsed dataset. Run funga_parse() first.",
        call. = FALSE)
  }

  if (rm_funga_database) {
    unlink("funga_download", recursive = TRUE)
  }

  list(taxon_df = taxon_df,
      distribution_df = distribution_df,
      speciesprofile_df = speciesprofile_df)
}


#_______________________________________________________________________________
# Collapse the (potentially many-rows-per-taxon) distribution and
# speciesprofile tables down to one row per taxon id, concatenating distinct
# values per column with " | ", then left-join them onto the taxon records.
.funga_append_distribution_speciesprofile <- function(records, distribution_df, speciesprofile_df) {

  collapse_col <- function(x) {
    x <- unique(trimws(as.character(stats::na.omit(x))))
    x <- x[nzchar(x)]
    if (length(x) == 0) NA_character_ else paste(x, collapse = " | ")
  }

  ids <- unique(records$id)
  original_id_order <- records$id

  if (!is.null(distribution_df) && nrow(distribution_df) > 0 && "id" %in% names(distribution_df)) {
    dist_sub <- distribution_df[distribution_df$id %in% ids, , drop = FALSE]
    dist_sub$state <- .arg_check_state(gsub("^BR-", "", dist_sub$locationID), return_abbrev = FALSE)
    dist_cols <- intersect(c("state", "phytogeographicDomain", "endemism"), names(dist_sub))
    if (nrow(dist_sub) > 0 && length(dist_cols) > 0) {
      dist_agg <- stats::aggregate(dist_sub[dist_cols], by = list(id = dist_sub$id),
                                   FUN = collapse_col)
      records <- merge(records, dist_agg, by = "id", all.x = TRUE, sort = FALSE)
    }
  }

  if (!is.null(speciesprofile_df) && nrow(speciesprofile_df) > 0 && "id" %in% names(speciesprofile_df)) {
    sp_sub <- speciesprofile_df[speciesprofile_df$id %in% ids, , drop = FALSE]
    sp_cols <- intersect(c("lifeForm", "habitat", "vegetationType"), names(sp_sub))
    if (nrow(sp_sub) > 0 && length(sp_cols) > 0) {
      sp_agg <- stats::aggregate(sp_sub[sp_cols], by = list(id = sp_sub$id),
                                 FUN = collapse_col)
      records <- merge(records, sp_agg, by = "id", all.x = TRUE, sort = FALSE)
    }
  }

  records[match(original_id_order, records$id), , drop = FALSE]
}
