% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/period_averager.R
\name{period_averager}
\alias{period_averager}
\title{Obtain period midpoints and average daily rates for count data}
\usage{
period_averager(
  data,
  count_col = "cases_this_period",
  start_col = "period_start_date",
  end_col = "period_end_date",
  norm_col = NULL,
  norm_const = 1e+05,
  keep_raw = TRUE,
  keep_cols = names(data)
)
}
\arguments{
\item{data}{Data frame with rows at minimum containing period start and end
dates and a count variable.}

\item{count_col}{Character, name of count data column.}

\item{start_col}{Character, name of start date column.}

\item{end_col}{Character, name of end date column.}

\item{norm_col}{Character, name of column giving data for normalization.
A good option is often \code{population_reporting}, which is a column in many
datasets containing the total size of the reference population for the count
data. To avoid normalization set \code{norm_col} to \code{NULL}, which is
the default.}

\item{norm_const}{Numeric value for multiplying the \code{daily_rate} column
if a \code{norm_col} is supplied. By default this is \code{1e5}, which corresponds to
\code{daily_rate} having units of \verb{count per day per 100,000 individuals} if the
\code{norm_col} represents the reference population size.}

\item{keep_raw}{Logical value indicating whether to force all \verb{*_col}
columns in the output, even if they are not specified in \code{keep_cols}, and
to place them at the beginning of the columns list. The default is \code{TRUE}.}

\item{keep_cols}{Character vector containing the names of columns in the
input \code{data} to retain in the output. All columns are retained by default.}
}
\value{
Data frame containing the following fields.
\itemize{
\item Columns from the original dataset specified using \code{keep_raw} and
\code{keep_cols}.
\item \code{year} : Year of the \code{period_start_date}.
\item \code{num_days} : Length of the period in days from the beginning of the
\code{period_start_date} to the end of the \code{period_end_date}.
\item \code{period_mid_time} : Timestamp of the middle of the period.
\item \code{period_mid_date} : Date containing the \code{period_mid_time}.
\item \code{daily_rate} : Daily count rate, which by default is given by
\code{daily_rate = count_col / num_days}. If the name of
\code{norm_col} is specified then
\code{daily_rate = norm_const * count_col / num_days / norm_col}.
When interpreting these formulas, please keep in mind that
\code{norm_const} is a numeric constant, \code{num_days} is a derived
numeric column, and \code{count_col} and \code{norm_col} are columns
supplied within the input \code{data} object.
}
}
\description{
Obtain period midpoints and average daily rates for count data
}
\examples{
set.seed(666)
data <- data.frame(disease = "senioritis"
 , period_start_date = seq(as.Date("2023-04-03"), as.Date("2023-06-05"), by = 7)
 , period_end_date = seq(as.Date("2023-04-09"), as.Date("2023-06-11"), by = 7)
 , cases_this_period = sample(0:100, 10, replace = TRUE)
 , location = "college"
)

period_averager(data, keep_raw = TRUE, keep_cols = c("disease", "location"))

}
\concept{periods}
\concept{time_periods}
