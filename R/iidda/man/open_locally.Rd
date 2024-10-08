% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{open_locally}
\alias{open_locally}
\alias{open_resources_locally}
\alias{open_all_resources_locally}
\alias{open_scans_locally}
\alias{open_digitizations_locally}
\title{Open a Path on Mac OS or Windows}
\usage{
open_locally(urls, command = "open", args = character())

open_resources_locally(
  id,
  type = c("scans", "digitizations", "prep-scripts", "access-scripts")
)

open_all_resources_locally(id)

open_scans_locally(id)

open_digitizations_locally(id)
}
\arguments{
\item{urls}{Character vector of GitHub URLs in blob storage}

\item{command}{Command-line function to use to open the file (not
applicable on Windows systems.}

\item{args}{Additional options to pass to \code{command} (ignored on
Windows systems).}

\item{id}{Resource ID.}

\item{type}{Type of resource.}
}
\description{
Open a Path on Mac OS or Windows
}
\section{Functions}{
\itemize{
\item \code{open_resources_locally()}: Open IIDDA pipeline resources locally.

\item \code{open_all_resources_locally()}: Open all pipeline resources regardless of
resource type.

\item \code{open_scans_locally()}: Open scans locally.

\item \code{open_digitizations_locally()}: Open digitizations locally.

}}
