% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_tools.R
\name{get_firsts}
\alias{get_firsts}
\title{Get Firsts}
\usage{
get_firsts(l, key)
}
\arguments{
\item{l}{A list of lists of lists}

\item{key}{Name of focal sublist (TODO: needs better description/motivation)}
}
\description{
Get the first item in each sublist of sublists (ugh ... I know).
}
\examples{
l = list(
  a = list(
    A = list(
      i = 1,
      ii = 2
    ),
    B = list(
      i = 3,
      ii = 4
    )
 ),
 b = list(
    A = list(
      i = 5,
      ii = 6
    ),
    B = list(
      i = 7,
      ii = 8
    )
  )
)
get_firsts(l, "A")
get_firsts(l, "B")
}
