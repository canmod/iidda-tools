% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_prep.R
\name{union_series}
\alias{union_series}
\title{Union Time Series}
\usage{
union_series(x, y, overlap = TRUE, time_variable = "period_end_date")
}
\arguments{
\item{x}{first data frame containing time series data}

\item{y}{second data frame containing time series data}

\item{overlap}{boolean to indicate if `x` should get priority with overlapping time periods in `y`.
If `TRUE` the returned data frame will contain all data from `x`, and the filtered `y` data that does
not overlap with `x`. If FALSE, a union between `x` and `y` is returned.}

\item{time_variable}{column name of time variable in `x` and `y`, default is "period_end_date"}
}
\value{
combined `x` and `y` data frames with optional filtering for overlaps
}
\description{
Combine two time series data sets with the option to handle overlapping time periods.
This is particularly useful for data sets that come from two sources (ex. LBoM and RG).
Assumes both data sets have the same number of columns with the same names.
}
