% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repo_paths.R
\name{list_dependency_ids_for_source}
\alias{list_dependency_ids_for_source}
\title{List Dependency IDs for Source}
\usage{
list_dependency_ids_for_source(
  source,
  type = c("PrepScripts", "Scans", "Digitizations", "AccessScripts")
)
}
\arguments{
\item{source}{IIDDA source ID, which should correspond to
metadata in `metadata/sources/souce.json` and a folder in `pipelines`.}

\item{type}{Type of dependency.}
}
\description{
List Dependency IDs for Source
}
