IIDDA R Package
---------------

<!-- badges: start -->
[![R-CMD-check](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Installation 
```
remotes::install_github('canmod-eidm/iidda-tools', subdir = 'R/iidda')
```

## Configuring Data Harmonization

The `tools` directory contains CSV files that clarify how historical inconsistencies in naming can be harmonized. For example, NFLD and Newfoundland can both be represented using the `iso-3166-2` standard as CA-NL. The `build_harmonization_data.R` script can be used to push changes to these CSV files to the `data` directory of this package so that it can leverage the latest harmonization rules.
