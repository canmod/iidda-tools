% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_prep.R
\name{iidda_plot_heatmap}
\alias{iidda_plot_heatmap}
\title{Plot Heatmap}
\usage{
iidda_plot_heatmap(
  plot_object,
  data = NULL,
  series_variable = "deaths",
  start_year_variable = "Year",
  end_year_variable = "End Year",
  start_day_variable = "Day of Year",
  end_day_variable = "End Day of Year",
  colour_trans = "log2",
  NA_colour = "black",
  palette_colour = "RdGy",
  ...
)
}
\arguments{
\item{plot_object}{a `ggplot2` plot object}

\item{data}{data frame containing data prepped for yearly vs. weekly heatmaps, typically output from
`iidda_prep_heatmap()`. If `NULL` data is inherited from `plot_object`.}

\item{series_variable}{column name of series variable in `data`, default is "deaths"}

\item{start_year_variable}{column name of time variable containing the year of the starting period, defaults to "Year"}

\item{end_year_variable}{column name of time variable containing the year of the ending period, defaults to "End Year"}

\item{start_day_variable}{column name of time variable containing the day of the starting period, defaults to "Day of Year"}

\item{end_day_variable}{column name of time variable containing the day of the ending period, defaults to "End Day of Year"}

\item{colour_trans}{string indicating colour transformation, one of "log2", "sqrt" or "linear"}

\item{NA_colour}{colour for `NA` values, defaults to "black"}

\item{palette_colour}{colour of heatmap palette, defaults to "RdGy". Should specify what type of palette colours
are accepted by this argument.}

\item{...}{Not currently used.}
}
\value{
a ggplot2 plot object containing a yearly vs. weekly heatmap of time series data
}
\description{
Add a yearly vs. weekly heatmap to an exiting ggplot plot object. Graphical choices were made to closely reflect
plots generated with`LBoM::seasonal_heat_map`.
}
\concept{plotting_functions}
