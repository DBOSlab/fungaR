#' Get child taxa from Flora e Funga do Brasil
#'
#' Returns all child taxa (species, subspecies, varieties, etc.) for a given
#' taxonomic name at a specified rank. Useful for getting all species in a genus,
#' all genera in an order, all species in a family, etc.
#'
#' @param taxon_name Character. Scientific name to search for (e.g.,
#'   "Trichoderma", "Xylariaceae", "Agaricales").
#'
#' @param rank Character. The taxonomic rank of the input name. Options:
#'   \code{"division"}, \code{"order"}, \code{"family"}, \code{"genus"},
#'   \code{"species"}. Default is \code{"genus"}. Fungi are classified in FFB
#'   by Division (phylum-equivalent, e.g. Ascomycota, Basidiomycota) rather
#'   than Class, and FFB does not register a standalone taxon record for
#'   family (or class) for the Fungi kingdom - \code{family} is stored only as
#'   a classification column on genus/species records. \code{rank = "family"}
#'   is still supported here, but internally it matches directly on that
#'   column rather than walking the parent-child hierarchy (see Details).
#'
#' @param child_rank Character or NULL. The taxonomic rank(s) to return as children.
#'   If a single rank is provided (e.g., \code{"species"}), returns only that rank.
#'   If \code{NULL} (default), returns ALL descendant ranks (e.g., for a genus:
#'   species, subspecies, varieties, etc.). Options: \code{"division"},
#'   \code{"order"}, \code{"family"}, \code{"genus"}, \code{"species"},
#'   \code{"subspecies"}, \code{"variety"}, \code{"form"}.
#'
#' @param include_synonyms Logical. If \code{TRUE}, includes synonym names
#'   (\code{taxonomicStatus == "SINONIMO"}) in addition to accepted names.
#'   Default is \code{FALSE}.
#'
#' @param version Character. FFB dataset version to use. Defaults to
#'   \code{"latest"}. Passed to \code{\link{funga_download}} and
#'   \code{\link{funga_parse}}.
#'
#' @param rm_funga_database Logical. If \code{TRUE}, the downloaded FFB database
#'   folder (\code{"funga_download"}) is deleted after the search is complete.
#'   If \code{FALSE} (default), the database is kept on disk. Keeping the
#'   database is **recommended** because subsequent searches will reuse the
#'   existing download, checking if the stored version matches the requested
#'   \code{version}. If the stored version is outdated or different from the
#'   requested version, the function automatically re-downloads the correct
#'   version. This caching behavior significantly speeds up repeated searches.
#'
#' @param verbose Logical. If \code{TRUE} (default), prints informative progress
#'   messages during parsing. If \code{FALSE}, runs quietly.
#'
#' @return A data frame containing child taxa that match the requested parent
#'   taxon after the applied filters.
#'
#' @details
#' For \code{rank \%in\% c("division", "order", "genus", "species")}, the
#' function first finds the parent taxon row, then returns all children that
#' have the parent's \code{taxonID} in their lineage (\code{parentNameUsageID}),
#' walking down the real FFB hierarchy for fungi: Division -> Order -> Genus ->
#' Species -> Subspecies/Variety/Form.
#'
#' For \code{rank = "family"}, there is no such row to walk from (FFB does not
#' give Fungi family-rank taxon records), so genus/species/infraspecific
#' records are instead matched directly on their \code{family} classification
#' column. Likewise, requesting \code{child_rank = "family"} from a division or
#' order returns a small \emph{derived} summary data frame (one row per unique
#' family found among the genus-level descendants, with the count of genera in
#' each), rather than real FFB taxon rows, since no such rows exist.
#'
#' **Child rank behavior:**
#' \itemize{
#'   \item If \code{child_rank = NULL}: Returns ALL descendant ranks
#'   \item If \code{child_rank = "species"}: Returns only species (direct children)
#'   \item If \code{child_rank = "subspecies"}: Returns only subspecies
#' }
#'
#' @section Database caching behavior:
#' The FFB dataset is downloaded only once and cached locally in the
#' \code{"funga_download"} folder. On subsequent calls:
#' \itemize{
#'   \item If \code{rm_funga_database = FALSE} (default), the function checks if
#'     the cached version matches the requested \code{version}. If yes, it
#'     reuses the existing download; if not, it downloads the correct version.
#'   \item If \code{rm_funga_database = TRUE}, the database is deleted after
#'     each search, forcing a fresh download on every call (not recommended
#'     for repeated searches).
#' }
#'
#' @examples
#' \dontrun{
#' # Get all species in a genus (direct children)
#' species <- funga_get_children_taxa(
#'   taxon_name = "Trichoderma",
#'   rank = "genus",
#'   child_rank = "species"
#' )
#'
#' # Get ALL descendants (species, subspecies, varieties)
#' all_descendants <- funga_get_children_taxa(
#'   taxon_name = "Trichoderma",
#'   rank = "genus",
#'   child_rank = NULL
#' )
#'
#' # Get all genera in a family (including synonyms) -- matched via the
#' # 'family' classification column, since FFB has no family-rank row for Fungi
#' genera <- funga_get_children_taxa(
#'   taxon_name = "Xylariaceae",
#'   rank = "family",
#'   child_rank = "genus",
#'   include_synonyms = TRUE
#' )
#'
#' # Get all genera in an order
#' genera_in_order <- funga_get_children_taxa(
#'   taxon_name = "Agaricales",
#'   rank = "order",
#'   child_rank = "genus"
#' )
#' }
#'
#' @export

funga_get_children_taxa <- function(taxon_name = NULL,
                                    rank = c("genus", "division", "order", "family", "species"),
                                    child_rank = NULL,
                                    include_synonyms = FALSE,
                                    version = "latest",
                                    rm_funga_database = FALSE,
                                    verbose = TRUE) {

  # ============================================================
  # INPUT VALIDATION
  # ============================================================

  if (is.null(taxon_name)) {
    stop("'taxon_name' must be provided.", call. = FALSE)
  }

  rank <- match.arg(rank)

  # Valid child ranks ('family' is handled as a special case - see below)
  valid_child_ranks <- c("division", "order", "family", "genus", "species", "subspecies", "variety", "form")

  if (!is.null(child_rank) && !child_rank %in% valid_child_ranks) {
    stop(sprintf("Invalid child_rank '%s'. Valid options: %s",
                 child_rank, paste(valid_child_ranks, collapse = ", ")),
         call. = FALSE)
  }

  if (!is.null(child_rank) && rank == "family" && child_rank == "family") {
    stop("'child_rank' cannot be 'family' when 'rank' is also 'family'.", call. = FALSE)
  }

  # ============================================================
  # DEFINE TAXONOMIC HIERARCHY
  # ============================================================

  # FFB's real row-level hierarchy for Fungi (confirmed empirically): Division
  # -> Order -> Genus -> Species -> Subspecies/Variety/Form. There is no
  # Class, Family, or Tribe taxonRank record for Fungi - 'family' is only a
  # classification column attached to genus/species/infraspecific rows, and
  # is handled separately below rather than through this recursive chain.
  rank_map <- c(
    division = "DIVISAO",
    order = "ORDEM",
    genus = "GENERO",
    species = "ESPECIE",
    subspecies = "SUB_ESPECIE",
    variety = "VARIEDADE",
    form = "FORMA"
  )

  rank_order <- c("division", "order", "genus", "species", "subspecies", "variety", "form")

  # Determine child ranks to retrieve (only meaningful for rank != "family";
  # the "family" parent case is handled entirely separately further below)
  if (rank != "family") {
    parent_index <- which(rank_order == rank)

    if (is.null(child_rank)) {
      # Get ALL descendant ranks (below parent)
      child_ranks_to_get <- rank_order[(parent_index + 1):length(rank_order)]

      if (length(child_ranks_to_get) == 0) {
        warning(sprintf("No descendant ranks found for rank '%s'", rank), call. = FALSE)
        return(data.frame())
      }

      if (verbose) {
        message(sprintf("Retrieving ALL descendant ranks: %s",
                        paste(child_ranks_to_get, collapse = " \u2192 ")))
      }
    } else if (child_rank == "family" && rank %in% c("division", "order")) {
      # Special case handled after the descendant genera are collected below.
      # 'family' only makes sense as a child concept below division/order
      # (it sits, conceptually, between order and genus); from genus or
      # species it is not a lower rank at all, so it falls through to the
      # standard rank_order lookup below and correctly errors.
      child_ranks_to_get <- "genus"
    } else {
      # Get only the specified child rank
      child_index <- which(rank_order == child_rank)

      if (length(child_index) == 0 || child_index <= parent_index) {
        stop(sprintf("child_rank '%s' must be lower than parent rank '%s'",
                     child_rank, rank), call. = FALSE)
      }

      child_ranks_to_get <- child_rank
    }
  }

  # ============================================================
  # DOWNLOAD AND PARSE FFB DATA
  # ============================================================

  if (verbose) message("\nDownloading and parsing Flora e Funga do Brasil data...")

  funga_download(version = version,
                dir = "funga_download",
                verbose = verbose)
  dwca <- funga_parse(path = "funga_download",
                      version = version,
                      verbose = verbose)

  ffb_taxa <- dwca[[list.files("funga_download")]][["data"]][["taxon.txt"]]

  # ============================================================
  # FIND THE PARENT TAXON (or, for rank = "family", the member rows)
  # ============================================================

  if (rank == "family") {

    # FFB has no standalone FAMILIA-rank row for Fungi: 'family' only exists
    # as a classification column on genus/species/infraspecific rows, so we
    # match directly on that column instead of a parentNameUsageID row.
    family_rows <- which(ffb_taxa$family == taxon_name)

    if (length(family_rows) == 0) {
      stop(sprintf("Taxon '%s' with rank 'family' not found in FFB database.",
                   taxon_name), call. = FALSE)
    }

    family_member_ids <- ffb_taxa$id[family_rows]

    if (verbose) {
      message(sprintf("\nFound family '%s' (matched via classification column, %d member record(s))",
                      taxon_name, length(family_rows)))
    }

    if (is.null(child_rank)) {
      children <- ffb_taxa[ffb_taxa$id %in% family_member_ids, ]
    } else {
      ffb_child_rank <- rank_map[child_rank]
      children <- ffb_taxa[ffb_taxa$id %in% family_member_ids &
                             ffb_taxa$taxonRank == ffb_child_rank, ]
    }

  } else {

    ffb_parent_rank <- rank_map[rank]

    # Find parent taxon
    if (rank == "species") {
      parent_rows <- which(ffb_taxa$taxonName == taxon_name &
                             ffb_taxa$taxonRank == ffb_parent_rank)
    } else if (rank == "division") {
      parent_rows <- which(ffb_taxa$phylum == taxon_name &
                             ffb_taxa$taxonRank == ffb_parent_rank)
    } else if (rank == "order") {
      parent_rows <- which(ffb_taxa$order == taxon_name &
                             ffb_taxa$taxonRank == ffb_parent_rank)
    } else if (rank == "genus") {
      parent_rows <- which(ffb_taxa$genus == taxon_name &
                             ffb_taxa$taxonRank == ffb_parent_rank)
    }

    if (length(parent_rows) == 0) {
      stop(sprintf("Taxon '%s' with rank '%s' not found in FFB database.",
                   taxon_name, rank), call. = FALSE)
    }

    parent_taxon <- ffb_taxa[parent_rows[1], ]
    parent_id <- parent_taxon$id

    if (verbose) {
      parent_display_name <- if (!is.na(parent_taxon$taxonName)) {
        parent_taxon$taxonName
      } else {
        parent_taxon$scientificName
      }
      message(sprintf("\nFound parent: %s (%s [%s])",
                      parent_display_name,
                      parent_taxon$taxonRank,
                      rank))
    }

    # ============================================================
    # FIND CHILD TAXA (RECURSIVELY IF NEEDED)
    # ============================================================

    # Function to find children recursively
    find_children_recursive <- function(parent_ids, target_ranks, current_depth = 0) {
      if (length(target_ranks) == 0 || length(parent_ids) == 0) {
        return(data.frame())
      }

      # Get the next rank to retrieve
      next_rank <- target_ranks[1]
      ffb_next_rank <- rank_map[next_rank]

      # Find direct children of current parents
      direct_children <- ffb_taxa[ffb_taxa$parentNameUsageID %in% parent_ids &
                                  ffb_taxa$taxonRank == ffb_next_rank, ]

      if (nrow(direct_children) == 0) {
        return(data.frame())
      }

      if (verbose && current_depth == 0) {
        message(sprintf("  Found %d %s", nrow(direct_children), next_rank))
      }

      # If there are more ranks to retrieve, continue recursively
      if (length(target_ranks) > 1) {
        remaining_ranks <- target_ranks[-1]
        child_ids <- as.character(direct_children$id)
        deeper_children <- find_children_recursive(child_ids, remaining_ranks, current_depth + 1)

        if (nrow(deeper_children) > 0) {
          direct_children <- rbind(direct_children, deeper_children)
        }
      }

      return(direct_children)
    }

    # Start recursive search
    children <- find_children_recursive(parent_id, child_ranks_to_get)

    # Fallback: if no children found via parentNameUsageID, try direct filter
    if (nrow(children) == 0 && rank == "genus" && "species" %in% child_ranks_to_get) {
      children <- ffb_taxa[ffb_taxa$genus == taxon_name &
                           ffb_taxa$taxonRank == rank_map["species"], ]
      if (verbose && nrow(children) > 0) {
        message(sprintf("  Found %d species via genus column fallback", nrow(children)))
      }
    }

    # Special case: child_rank = "family" has no real FFB row to return, so
    # derive a small summary of unique families among the genus descendants
    # collected above (recall child_ranks_to_get was forced to "genus" for
    # this case).
    if (!is.null(child_rank) && child_rank == "family") {
      if (nrow(children) == 0) {
        children <- data.frame()
      } else {
        fam_tab <- as.data.frame(table(family = children$family, order = children$order),
                                 stringsAsFactors = FALSE)
        names(fam_tab)[names(fam_tab) == "Freq"] <- "n_genera"
        fam_tab <- fam_tab[fam_tab$n_genera > 0 & fam_tab$family != "", ]
        rownames(fam_tab) <- NULL
        if (verbose && nrow(fam_tab) > 0) {
          message(sprintf("  Derived %d unique famil%s from genus-level descendants (no FFB family-rank row exists for Fungi)",
                          nrow(fam_tab), if (nrow(fam_tab) == 1) "y" else "ies"))
        }
        if (rm_funga_database) unlink("funga_download", recursive = TRUE)
        if (nrow(fam_tab) == 0) {
          warning(sprintf("No families found for %s '%s'", rank, taxon_name), call. = FALSE)
        }
        return(fam_tab)
      }
    }

  }

  if (nrow(children) == 0) {
    warning(sprintf("No children found for %s '%s'", rank, taxon_name), call. = FALSE)
    if (rm_funga_database) {
      unlink("funga_download", recursive = TRUE)
    }
    return(data.frame())
  }

  # ============================================================
  # FILTER CHILDREN
  # ============================================================

  # Filter by taxonomic status (accepted vs synonyms)
  if (!include_synonyms) {
    n_before <- nrow(children)
    children <- children[children$taxonomicStatus == "NOME_ACEITO", ]

    if (verbose && nrow(children) < n_before) {
      message(sprintf("  Filtered out %d synonyms", n_before - nrow(children)))
    }
  }

  # ============================================================
  # REMOVE EMPTY ROWS
  # ============================================================

  n_before <- nrow(children)
  rows_with_data <- rowSums(!is.na(children)) > 0
  children <- children[rows_with_data, ]

  if (verbose && nrow(children) < n_before) {
    message(sprintf("  Removed %d completely empty rows", n_before - nrow(children)))
  }

  if (rm_funga_database) {
  unlink("funga_download", recursive = TRUE)
  }

  if (verbose) {
    message(sprintf("\n\u2713 Returned %d child taxa for %s '%s'",
                    nrow(children), rank, taxon_name))
  }

  return(children)
}
