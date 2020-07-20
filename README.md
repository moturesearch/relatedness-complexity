# Relatedness, Complexity and Local Growth

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3951769.svg)](https://doi.org/10.5281/zenodo.3951769)

This repository contains the source material for "Relatedness, Complexity and Local Growth," which was published first as [Motu Working Paper 19-01](https://motu.nz/our-work/urban-and-regional/regions/relatedness-complexity-and-local-growth/) and subsequently in [*Regional Studies*](https://www.tandfonline.com/cres20).

## Workflow

We generate data, figures and tables by running the R scripts `code/data.R`, `code/figures.R` and `code/tables.R` (in that order).
We run each script in a fresh `relatedness-complexity.Rproj` instance within [RStudio](https://www.rstudio.com/).

### Data

[`data/raw/`](data/raw/) contains the raw data used in our analysis.

### Dependencies

Our analysis uses several R packages.
We identify these packages at the beginning of each R script in `code/` and in the log files in `logs/`.
All dependencies can be installed via

```r
# Install CRAN packages
cran_packages <- c(
  "ggraph",
  "igraph",
  "knitr",
  "kableExtra",
  "lfe",
  "reshape2",
  "sessioninfo",
  "stargazer",
  "tidygraph",
  "tidyverse"
)
install.packages(cran_packages)

# Install WGCNA
install.packages("BiocManager")
BiocManager::install("WGCNA")

# Install EconGeo
install.packages("remotes")
remotes::install_github("PABalland/EconGeo")
```

## License

[MIT](https://github.com/moturesearch/relatedness-complexity/blob/master/LICENSE)
