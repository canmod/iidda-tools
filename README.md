# iidda-tools

<!-- badges: start -->
[![iidda.api status badge](https://canmod.r-universe.dev/badges/iidda.api)](https://canmod.r-universe.dev/iidda.api)
[![R-CMD-check](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check-iidda.yaml/badge.svg)](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check-iidda.yaml)
[![R-CMD-check](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check-iidda.api.yaml/badge.svg)](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check-iidda.api.yaml)
[![R-CMD-check](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check-iidda.analysis.yaml/badge.svg)](https://github.com/stevencarlislewalker/iidda-tools/actions/workflows/R-CMD-check-iidda.analysis.yaml)
<!-- badges: end -->

Open toolchain for processing infectious disease datasets available through [IIDDA](https://github.com/canmod/iidda) and other repositories. The toolchain contains [R packages](R/README.md) and [Python packages and code](python/README.md).

## For Users

To install the required [IIDDA](https://github.com/canmod/iidda) [R packages](R/README.md), run:
```
make iidda-r-pkg
```

The current implementation first check if there are any changes or updates in the [Python packages and code](python/README.md) from [IIDDA tools](https://github.com/canmod/iidda-tools/tree/main) and pulls the updates and then starting the API. To start the `API` (assuming you've already set up [Python packages and code](python/README.md)), run:
```
make update-start-api
```

To start the `API` without checking for the updates, run `make start-api`

TODO

## For Developers


