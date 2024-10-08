% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/period_averager.R
\name{mid_dates_times}
\alias{mid_dates_times}
\alias{mid_dates}
\alias{mid_times}
\title{Period Mid-Dates and Mid-Times}
\usage{
mid_dates(start_date, end_date, period_length)

mid_times(start_date, end_date, period_length)
}
\arguments{
\item{start_date}{Vector of period starting dates}

\item{end_date}{Vector of period ending dates. If missing then
\code{period_length} is used to define the ends of the periods.}

\item{period_length}{Vector of integers giving the period length in days.
If missing then it is calculated using \code{\link{num_days}}.}
}
\description{
Compute a vector giving the mid-points of a vector of temporal periods,
defined by start dates and one of either a vector of end dates or a vector
of period lengths in days (see \code{\link{num_days}}). You can either
return a date, with \code{mid_dates}, or a date-time, with \code{mid_times}.
In addition to the type of return value (date vs time), the former rounds
down to the nearest date whereas the latter is accurate to the nearest hour
and so can account for uneven
}
\concept{periods}
