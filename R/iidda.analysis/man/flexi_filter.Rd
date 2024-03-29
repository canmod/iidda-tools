% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regex_query_tools.R
\name{flexi_filter}
\alias{flexi_filter}
\title{Get data where any field(s) match inputted strings}
\usage{
flexi_filter(
  strings,
  fields,
  response_type = "columns",
  data_type = "Communicable Disease Incidence",
  ...
)
}
\arguments{
\item{strings}{Character vector of regex strings to match}

\item{fields}{Character vector, names of field(s) to match}

\item{response_type}{Character, one of `metadata`, `columns`, or
`data_dictionary`}

\item{data_type}{Character, one of `cdi`, `CDI`, or `Communicable Disease
Incidence` for disease incidence time series datasets, or `pop`, `Pop`,  or
`Population` for historical demographic census datasets"}

\item{...}{Optional arguments passed to iidda.api functions}
}
\value{
Flat data with unique entries matching query
}
\description{
Get data where any field(s) match inputted strings
}
\details{
the `filter` function searches using a logical "OR" between strings
  within fields, but an "AND" between fields. Searches are fast and so more
  complex searches may best be handled on the fly, filtering or combining the
  data returned by individual, simpler searches.
}
\examples{
# get some viral diseases
strings <- c(".*vir.*", ".*Vir.*")
fields <- c("disease")
ttr <- flexi_filter(strings, fields)

head(ttr)

# some things might be listed as viral under disease_subclass instead.
# concatenate data from two `flexi_filter` calls
all_vir <- dplyr::bind_rows(ttr
, flexi_filter(strings, fields = "disease_subclass"))


head(all_vir)
nrow(all_vir)
# unfortunately place names are kind of a mess.
# Probably easiest to find the intersection between two datasets to filter
# by both disease and location

#Nova Scotia
nsDat <- flexi_filter(strings = c("Nova.*", "(^[nN].*)([[:punct:]]|[[:space:]])[sS]+.*")
     , fields = "location")

head(nsDat)
nrow(nsDat)

# viruses in nova scotia

ns_vir <- all_vir \%>\% dplyr::inner_join(nsDat)

head(ns_vir)
nrow(ns_vir)
}
\seealso{
\code{\link{query_options}}
}
