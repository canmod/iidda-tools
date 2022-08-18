# IIDDA R Packages

The `iidda` package contains general utilities that are consumed by the open data pipelines in https://github.com/davidearn/iidda.
```
remotes::install_github('canmod/iidda-tools', subdir = 'R/iidda')
```

The `iidda.analysis` package is a new development effort intended to contain functions for visualization, analysis, and harmonization of `iidda` datasets.
```
remotes::install_github('canmod/iidda-tools', subdir = 'R/iidda.analysis')
```

The `iidda.api` package is an R binding to the [iidda_api](../python/README.md)
```
remotes::install_github('canmod/iidda-tools', subdir = 'R/iidda.api')
```

This `iidda.api` package depends on our specific [fork](https://github.com/canmod/rapiclient/) of [`rapiclient`](https://github.com/bergant/rapiclient).
```
remotes::install_github('canmod/rapiclient')
```
