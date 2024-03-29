% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xlsx_diff.R
\name{xlsx_diff}
\alias{xlsx_diff}
\title{Compare Two Excel Files}
\usage{
xlsx_diff(path_one, path_two, ...)
}
\arguments{
\item{path_one}{Path to an Excel file.}

\item{path_two}{Path to an Excel file.}

\item{...}{Additional arguments to pass to \code{\link{xlsx_cells}}.}
}
\value{
Either `TRUE` if the two files are identical, or a list with the
following items.
* `all_equal` : Result of applying \code{\link{all.equal}} to the
data frames representing each Excel file.
* `in_both_but_different` : Data frame containing cells that are in both
Excel files but with different values.
* `in_one_only` : Data frame containing cells that are in the first
Excel file but not the second.
* `in_two_only` : Data frame containing cells that are in the second
Excel file but not the first.
}
\description{
Report on the differences between two xlsx files.
}
