# fungaR 1.0.0

## Initial Release

The first official release of the `fungaR` R package, designed to provide comprehensive access, analysis, and curation tools for fungal data from the Flora e Funga do Brasil (FFB) platform maintained by the Rio de Janeiro Botanical Garden.

### Features

- `funga_version()`: Retrieve metadata and check available dataset versions from the Flora e Funga do Brasil IPT data portal.
- `funga_download()`: Download taxonomic and distributional data in Darwin Core Archive (DwC-A) format for specific or all available versions.
- `funga_parse()`: Parse and organize locally downloaded FFB datasets, extracting structured information from DwC-A files and subsetting the combined plant/fungi archive down to the Fungi kingdom.
- `funga_records()`: Browse and filter the FFB fungal checklist directly by taxonomic, geographic, and trait-based criteria, without requiring an input name list. Downloads and parses the dataset automatically, reusing the local cache on repeated calls. The `habitat` argument doubles as substrate/host information for fungi.
- `funga_search()`: Resolve your own species name list against the FFB checklist, with exact matching first and fuzzy (Levenshtein-distance) matching as a fallback for typos, including synonym resolution.
- `funga_match()`: Compare two independent species name lists, aligning names that resolve to the same accepted taxon.
- `funga_get_children_taxa()`: Retrieve all child taxa (species, subspecies, varieties, genera, etc.) below a given taxonomic name and rank, following FFB's real fungi hierarchy (Division > Order > Genus > Species), with family handled via its classification column since FFB registers no standalone family-rank record for Fungi.
- `funga_mycobank_gap()`: Cross-check MycoBank's global name database against the FFB checklist for a given genus or order, flagging species-level names missing from FFB and optionally cross-referencing GBIF/speciesLink for Brazil occurrence evidence. Returns a structured spreadsheet including each name's original MycoBank URL, to assist taxonomic experts curating new records.
- `funga_distribution_gap()`: Cross-check GBIF, speciesLink, and the Reflora Virtual Herbarium (via `refloraR`) for specimen evidence of a taxon in Brazilian states not yet listed in FFB's official distribution, flagging candidate new state records.
- Automated data cleaning and standardization of taxon names, distribution data, and species profiles.
- Support for offline data analysis once datasets are downloaded.
- Integration with global mycological and biodiversity repositories (MycoBank, speciesLink, REFLORA, GBIF) for data curation workflows.
- Seamless integration with tidyverse packages for downstream analyses.

### Key Capabilities

- **Version Control**: Track, download, and parse specific dataset versions
- **Checklist Filtering**: Browse and filter the FFB fungal checklist by taxonomic, geographic, and trait-based criteria without an input name list
- **Name Resolution**: Exact and fuzzy matching of your own species lists against the FFB checklist, including synonym resolution
- **Taxonomic Hierarchy**: Retrieve child taxa at any rank, from division down to species, on FFB's real fungi hierarchy
- **Distribution Data**: Extract endemism status and phytogeographic domain information
- **Species Profiles**: Parse life form and substrate/host (via the `habitat` field) data
- **Fungal Data Curation**: Cross-check MycoBank, GBIF, speciesLink, and REFLORA to flag species and state records missing from FFB
- **Offline Analysis**: Work with downloaded data without internet connection

### Infrastructure

- MIT license
- Comprehensive test coverage with testthat
- Continuous integration via GitHub Actions
- Hosted documentation: [fungaR-website](https://dboslab.github.io/fungaR-website/), with How-To articles for every function and a full Portuguese translation (EN/PT language switcher)
- CRAN-ready package structure

### Workflow

The package supports a full workflow for working with Flora e Funga do Brasil fungal data:

1. Check available versions with `funga_version()`
2. Download datasets with `funga_download()`
3. Parse datasets with `funga_parse()`
4. Filter and retrieve checklist records with `funga_records()`
5. Resolve your own species names with `funga_search()` and `funga_match()`
6. Explore the taxonomic hierarchy with `funga_get_children_taxa()`
7. Curate new records with `funga_mycobank_gap()` and `funga_distribution_gap()`

Most functions download and parse the FFB dataset automatically and cache it locally, so `funga_download()`/`funga_parse()` rarely need to be called directly unless you want to inspect the raw data.

### Target Users

- Researchers analyzing Brazilian fungal diversity
- Taxonomic experts contributing to the Flora e Funga do Brasil
- Mycologists studying fungal distributions and biogeography
- Conservation biologists working with Brazilian fungi
- Educators and students in biodiversity informatics

### Feedback

Please report bugs or issues at:
<https://github.com/DBOSlab/fungaR/issues>

### Citation

Cardoso, D. 2026. fungaR: An R Package for Accessing, Analyzing, and Curating Fungal Data from the Flora e Funga do Brasil Platform. https://github.com/dboslab/fungaR
