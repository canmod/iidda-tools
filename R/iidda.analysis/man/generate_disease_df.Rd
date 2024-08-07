% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/canmod_cdi.R
\name{generate_disease_df}
\alias{generate_disease_df}
\title{Data for a Particular Disease}
\usage{
generate_disease_df(canmod_cdi, disease_name, years = NULL, add_gaps = TRUE)
}
\arguments{
\item{canmod_cdi}{Dataset from IIDDA of type `CANMOD CDI`.}

\item{disease_name}{Name to match in the `nesting_disease` column of a
`CANMOD CDI` dataset.}

\item{years}{If not `NULL`, a vector of years to keep in the output data.}

\item{add_gaps}{If `TRUE`, add records with `NA` in `cases_this_period`
that correspond to time-periods without any data.}
}
\description{
Data for a Particular Disease
}
