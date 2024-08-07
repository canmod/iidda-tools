% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalization_tools.R
\name{normalize_population}
\alias{normalize_population}
\title{Normalize Population}
\usage{
normalize_population(data, harmonized_population)
}
\arguments{
\item{data}{Tidy dataset with columns period_start_date, period_end_date, 
iso_3166_2}

\item{harmonized_population}{Harmonized population data from API
(load using api_hook$raw_csv(dataset_ids = "pop_ca_1871-2021_harmonized"))
or iidda-staging
(load from 
iidda-staging/derived_data/pop_ca_1871-2021_harmonized/pop_ca_1871-2021_harmonized/pop_ca_1871-2021_harmonized.csv)}
}
\value{
Tidy dataset joined with harmonized population.
}
\description{
Normalize Population
}
