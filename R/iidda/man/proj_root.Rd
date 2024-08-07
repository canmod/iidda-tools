% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repo_paths.R
\name{proj_root}
\alias{proj_root}
\alias{in_proj}
\title{Project Root}
\usage{
proj_root(filename = ".iidda", start_dir = getwd(), default_root = start_dir)

in_proj(filename = ".iidda", start_dir = getwd())
}
\arguments{
\item{filename}{String giving the name of the file that identifies the project.}

\item{start_dir}{Optional directory from which to start looking for `filename`.}

\item{default_root}{Project root to use if `filename` is not found.}
}
\description{
Find the root path of an IIDDA-associated project (or any project
with a file of a specific name in the root).
}
\details{
Recursively walk up the file tree from `start_dir` until `filename` is
found, and return the path to the directory containing `filename`. If
`filename` is not found, return `default_root`
}
\section{Functions}{
\itemize{
\item \code{in_proj()}: Is a particular directory inside a project as
indicated by `filename`.

}}
