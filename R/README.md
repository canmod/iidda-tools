# IIDDA R Packages and App

## `iidda`

The `iidda` package contains general utilities that are consumed by the open data pipelines in https://github.com/davidearn/iidda.
```
remotes::install_github('canmod/iidda-tools', subdir = 'R/iidda')
```

## `iidda.analysis`

The `iidda.analysis` package is a new development effort intended to contain functions for visualization, analysis, and harmonization of `iidda` datasets. This package is experimental and not ready for end users of any kind.
```
remotes::install_github('canmod/iidda-tools', subdir = 'R/iidda.analysis')
```

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

### Setup

This package depends on a running instance of the [IIDDA API](../python/README.md). Currently we do not have a deployed production version 😢, and so you will need to start your own development server for the API. The above link to the IIDDA API documentation provides instructions for doing this. Once you have a local server running the R bindings to it should work just fine.

### Usage

The `iidda.api` package exports the following three objects: `ops`, `ops_local`, and `ops_staging`. Each of these three objects contains the same set of API functions. The difference between the three varieties of `ops*` is that `ops` is bound to the production server, `ops_local` to a development server, and `ops_staging` to a staging server. Most users are intended to use `ops` but the production version is not yet up 😢, and for this reason `ops` is currently identical to `ops_local`. The `ops_staging` is exclusively for members of the IIDDA core team.

Each function in `ops` directly corresponds to one of the functions in the API itself. A link to interactive documentation for each function can be found (if it exists) in the three exported objects, `docs_url`, `docs_url_staging`, `docs_url_local`.

## `iidda.shiny`

Interactive tool for exploring IIDDA.

### Setup

* Get the `iidda.api` package working
* From RStudio click the "Run App" button
