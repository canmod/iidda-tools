% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_prep_tools.R
\name{filter_out_time_scales}
\alias{filter_out_time_scales}
\title{Filter out Time Scales}
\usage{
filter_out_time_scales(
  data,
  initial_group = c("iso_3166", "iso_3166_2", "disease", "nesting_disease"),
  final_group = c("basal_disease"),
  cleanup = TRUE
)
}
\arguments{
\item{data}{A tidy data set with a `time_scale` column.}

\item{initial_group}{Character vector naming columns for defining
the initial grouping used to compute the shortest time scales.}

\item{final_group}{Character vector naming columns for defining the final
grouping used to compute the longest of the shortest time scales.}

\item{cleanup}{Should intermediate columns be removed before returning the
output}
}
\value{
A data set only containing records with the best time scale.
}
\description{
Choose a single best `time_scale` for each year in a dataset, grouped by
nesting disease. This best `time_scale` is defined as the longest
of the shortest time scales in each location and sub-disease.
}
