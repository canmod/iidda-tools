% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalization_tools.R
\name{get_implied_zeros}
\alias{get_implied_zeros}
\title{Get Implied Zeros}
\usage{
get_implied_zeros(data)
}
\arguments{
\item{data}{A tidy data set with the following minimal set of columns:
`disease`, `nesting_disease`, `year`, `original_dataset_id`, `iso_3166_2`,
`basal_disease`, `time_scale`, `period_start_date`, `period_end_date`,
`period_mid_date`, `days_this_period`, `dataset_id`}
}
\value{
A tidy data set with inferred 0s.
}
\description{
Add zeros to data set that are implied by a '0' reported at a coarser timescale.
}
\concept{normalization}
