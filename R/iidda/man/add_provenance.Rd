% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_prep_tools.R
\name{add_provenance}
\alias{add_provenance}
\title{Add Provenance}
\usage{
add_provenance(tidy_data, tidy_dataset)
}
\arguments{
\item{tidy_data}{Data frame in IIDDA tidy form.}

\item{tidy_dataset}{The IIDDA identifier associated with the dataset for
which `tidy_data` serves as an intermediate object during its creation.}
}
\description{
Add provenance information to an IIDDA dataset, by creating columns
containing the scan and digitization IDs associated with each record.
}
