% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/string_tools.R
\name{summarise_diseases}
\alias{summarise_diseases}
\title{Summarise Diseases}
\usage{
summarise_diseases(data)
}
\arguments{
\item{data}{Data frame hopefully containing at least one of `disease` or
`historical_disease`. If all are missing then the output summary is
a blank string.}
}
\value{
A string summarizing the data in the columns.
}
\description{
Summarise disease name columns in an IIDDA dataset.
}
