# IIDDA Analysis R Package

<!-- badges: start -->
[![iidda status badge](https://canmod.r-universe.dev/badges/iidda.analysis)](https://canmod.r-universe.dev/iidda.analysis)
[![R-CMD-check](https://github.com/canmod/iidda-tools/actions/workflows/R-CMD-check-iidda.analysis.yaml/badge.svg)](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check-iidda.analysis.yaml)
<!-- badges: end -->

The `iidda.analysis` package contains functions for visualization, analysis, and harmonization of `iidda` datasets.

This package is not stable. You will likely be more interested in the [iidda.api](https://canmod.github.io/iidda-tools/iidda.api) package.

## Installation

### Stable Version

```
install.packages(c("iidda.analysis")
  , repos = c(
      "https://canmod.r-universe.dev"
    , "https://cran.r-project.org"
  )
)
```

### Developer Version

```
remotes::install_github('canmod/iidda-tools', subdir = 'R/iidda.analysis')
```
