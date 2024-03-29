% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_prep_age_groups.R
\name{fill_and_wrap}
\alias{fill_and_wrap}
\title{Fill Template and Wrap the Results}
\usage{
fill_and_wrap(re_templates, which_bound, purpose, prefix = "")
}
\arguments{
\item{re_templates}{a set of \code{re_template}s each passed to
\code{\link{fill_re_template}}}

\item{which_bound}{resolve the template to match lower or upper bounds,
neither (the default), or single}

\item{purpose}{character string indicating the purpose of the resulting
regular expression}

\item{prefix}{pattern to match at the beginning of the string that marks
the beginning of age information}
}
\description{
Convenience function to do \code{\link{fill_re_template}} and
\code{\link{wrap_age_patterns}} in one step.
}
