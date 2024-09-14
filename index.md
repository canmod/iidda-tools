# IIDDA Tools Documentation

* Get data from the archive directly into an R data frame using the [iidda.api](https://canmod.github.io/iidda-tools/iidda.api/) R package. 
* Tools for analyzing data from IIDDA using the [iidda.analysis](https://canmod.github.io/iidda-tools/iidda.analysis/) R package.
* Utilities for IIDDA data curation and access using the [iidda](https://canmod.github.io/iidda-tools/iidda/) R package.

## Installation

### Stable Versions

```
install.packages(c("iidda", "iidda.api", "iidda.analysis")
  , repos = c(
      "https://canmod.r-universe.dev"
    , "https://cran.r-project.org"
  )
)
```

### Developer Versions

```
remotes::install_github('canmod/iidda-tools', subdir = 'R/iidda')
remotes::install_github('canmod/iidda-tools', subdir = 'R/iidda.api')
remotes::install_github('canmod/iidda-tools', subdir = 'R/iidda.analysis')
```
