# IIDDA API R Package

<!-- badges: start -->
[![R-CMD-check](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check-iidda.api.yaml/badge.svg)](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check-iidda.api.yaml)
<!-- badges: end -->

Quick install:

## `iidda.api`

The `iidda.api` package is an R binding to the [iidda_api](../python/README.md). These bindings are for developers building R packages and Shiny Apps on top of IIDDA. Currently `iidda.analysis` and `iidda.shiny` both depend on `iidda.api`.

### Installation

This `iidda.api` package depends on our specific [fork](https://github.com/canmod/rapiclient/) of [`rapiclient`](https://github.com/bergant/rapiclient).
```
remotes::install_github('canmod/rapiclient')
```

Once this dependency and [others](iidda.api/DESCRIPTION) are installed, you can install the package itself using this command.
```
remotes::install_github('canmod/iidda-tools', subdir = 'R/iidda.api')
```

