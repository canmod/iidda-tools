# IIDDA R Packages and App

<!-- badges: start -->
[![R-CMD-check](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check-iidda.yaml/badge.svg)](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check-iidda.yaml)
[![R-CMD-check](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check-iidda.api.yaml/badge.svg)](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check-iidda.api.yaml)
[![R-CMD-check](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check-iidda.analysis.yaml/badge.svg)](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check-iidda.analysis.yaml)
<!-- badges: end -->

## Where to Start

Lots of stuff here, but [this](https://canmod.github.io/iidda-tools/iidda.api) is probably where you want to start.

## Installing all R Packages

```
install.packages(c("iidda", "iidda.api", "iidda.analysis")
  , repos = c(
      "https://canmod.r-universe.dev"
    , "https://cran.r-project.org"
  )
)
```

## Shiny

The [iidda.shiny](R/iidda.shiny) directory contains an interactive tool for exploring IIDDA. This is not deployed anywhere but you should be able to use it after installing `iidda.api`.
