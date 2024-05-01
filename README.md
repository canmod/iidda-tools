# iidda-tools

<!-- badges: start -->
[![iidda.api status badge](https://canmod.r-universe.dev/badges/iidda.api)](https://canmod.r-universe.dev/iidda.api)
[![R-CMD-check](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check-iidda.yaml/badge.svg)](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check-iidda.yaml)
[![R-CMD-check](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check-iidda.api.yaml/badge.svg)](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check-iidda.api.yaml)
[![R-CMD-check](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check-iidda.analysis.yaml/badge.svg)](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check-iidda.analysis.yaml)
<!-- badges: end -->

Open toolchain for processing infectious disease datasets available through [IIDDA](https://github.com/canmod/iidda) and other repositories. The toolchain contains [R packages](R/README.md) and [Python packages and code](python/README.md).

## For Users

The API can be found [here](https://math.mcmaster.ca/iidda/api/docs)

To install the [IIDDA](https://github.com/canmod/iidda) [R packages](R/README.md), run:
```
install.packages(c("iidda", "iidda.api", "iidda.analysis")
  , repos = c(
      "https://canmod.r-universe.dev"
    , "https://cran.r-project.org"
  )
)
```

## For Developers

TODO
