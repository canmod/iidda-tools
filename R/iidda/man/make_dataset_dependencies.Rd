% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contribution_helpers.R
\name{make_dataset_dependencies}
\alias{make_dataset_dependencies}
\title{Make Dataset Dependencies}
\usage{
make_dataset_dependencies(tidy_dataset, paths)
}
\arguments{
\item{tidy_dataset}{Dataset ID for which dependencies are being
declared.}

\item{paths}{Relative paths to dependencies.}
}
\description{
Create a dependency file for a dataset. This file is created once and
any edits should be made manually to the created file.
}
