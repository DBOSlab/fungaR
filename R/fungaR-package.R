#' The fungaR package provides a comprehensive interface to the Flora e Funga do
#' Brasil (FFB) Platform, maintained by the Rio de Janeiro Botanical Garden.
#' It offers tools for downloading, parsing, and analyzing taxonomic and
#' distributional data published in Darwin Core Archive (DwC-A) format.
#'
#' Beyond data retrieval, fungaR also assists taxonomic experts contributing to
#' the FFB by simplifying the integration of new fungal species names and records
#' from global mycological and biodiversity repositories such as MycoBank, Index
#' Fungorum, speciesLink, and GBIF. The package is designed to streamline both
#' data exploration and data curation workflows, integrating seamlessly with the
#' tidyverse for downstream analyses and reproducible research pipelines.
#'
#' The package's main functions \code{\link{funga_version}} retrieves metadata
#' about available dataset versions from the FFB IPT data portal.
#' \code{\link{funga_download}} downloads specific or all available dataset
#' versions in DwC-A format, while \code{\link{funga_parse}} parses and
#' organizes the locally downloaded data for analysis. These functions together
#' provide a complete workflow for accessing and analyzing Brazilian fungal
#' diversity data.
#'
#' fungaR also provides three functions dedicated to fungal-specific curation
#' work: \code{\link{funga_mycobank_gap}}, which cross-checks a given genus or
#' order against MycoBank's global name database and its own locality
#' evidence to flag fungal species MycoBank places in Brazil but that are
#' still missing from FFB; \code{\link{funga_mycobank_records}}, which
#' retrieves every available MycoBank name for a given species, genus, or
#' order, filtered by default to those with MycoBank Brazil evidence,
#' regardless of whether they are already registered in FFB; and
#' \code{\link{funga_distribution_gap}}, which checks speciesLink and the
#' Reflora Virtual Herbarium for specimen evidence of a taxon in Brazilian
#' states not yet listed in its official FFB distribution.
#'
#' For researchers, fungaR enables comprehensive analyses of taxonomic
#' distributions, endemism patterns, phytogeographic domains, life forms,
#' habitats/substrates, and vegetation types across Brazil. For taxonomic
#' experts, it facilitates data curation and integration of new records into
#' the FFB platform.
#'
#' For the most recent version of fungaR, please visit the package's
#' GitHub repository (\url{https://github.com/dboslab/fungaR}).
#'
#' @name fungaR-package
#'
#' @aliases fungaR-package
#'
#' @title Tools for Accessing, Analyzing, and Curating Data from the Flora e Funga do Brasil Platform
#'
#' @author \strong{Domingos Cardoso}\cr
#' (ORCID: \href{https://orcid.org/0000-0001-7072-2656}{0000-0001-7072-2656};
#' email: \email{domingoscardoso@@jbrj.gov.br};
#' Rio de Janeiro Botanical Garden, Brazil)
#'
#' @keywords package
#'
#' @details \tabular{ll}{
#' Package: \tab fungaR\cr
#' Type: \tab Package\cr
#' Version: \tab 1.0.0\cr
#' Date: \tab 2025-02-25\cr
#' License: \tab MIT\cr
#' }
#'
#' @references Cardoso, D. (2025). fungaR: An R Package for
#' Accessing, Analyzing, and Curating Data from the Flora e Funga do Brasil
#' Platform.
#'
#' @seealso
#' Useful links:
#' \itemize{
#'   \item \url{https://dboslab.github.io/fungaR-website/} - Package documentation website
#'   \item \url{https://github.com/DBOSlab/fungaR} - Source code and issue tracker
#'   \item \url{https://floradobrasil.jbrj.gov.br/} - Flora e Funga do Brasil platform
#'   \item \url{https://ipt.jbrj.gov.br/jbrj} - FFB IPT data portal
#' }
#'
#' @importFrom rlang .data
"_PACKAGE"

NULL
