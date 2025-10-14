#' Obtain period midpoints and average daily rates for count data
#'
#' @md
#' @param data Data frame with rows at minimum containing period start and end
#' dates and a count variable.
#' @param count_col Character, name of count data column.
#' @param start_col Character, name of start date column.
#' @param end_col Character, name of end date column.
#' @param norm_col Character, name of column giving data for normalization.
#' A good option is often `population_reporting`, which is a column in many
#' datasets containing the total size of the reference population for the count
#' data. To avoid normalization set \code{norm_col} to \code{NULL}, which is
#' the default.
#' @param norm_const Numeric value for multiplying the `daily_rate` column
#' if a `norm_col` is supplied. By default this is `1e5`, which corresponds to
#' `daily_rate` having units of `count per day per 100,000 individuals` if the
#' `norm_col` represents the reference population size.
#' @param keep_raw Logical value indicating whether to force all `*_col`
#' columns in the output, even if they are not specified in `keep_cols`, and
#' to place them at the beginning of the columns list. The default is `TRUE`.
#' @param keep_cols Character vector containing the names of columns in the
#' input `data` to retain in the output. All columns are retained by default.
#'
#' @concept periods
#' @returns Data frame containing the following fields.
#' * Columns from the original dataset specified using `keep_raw` and
#' `keep_cols`.
#' * `year` : Year of the `period_start_date`.
#' * `num_days` : Length of the period in days from the beginning of the
#'                `period_start_date` to the end of the `period_end_date`.
#' * `period_mid_time` : Timestamp of the middle of the period.
#' * `period_mid_date` : Date containing the `period_mid_time`.
#' * `daily_rate` : Daily count rate, which by default is given by
#'                  `daily_rate = count_col / num_days`. If the name of
#'                  `norm_col` is specified then
#'                  `daily_rate = norm_const * count_col / num_days / norm_col`.
#'                  When interpreting these formulas, please keep in mind that
#'                  `norm_const` is a numeric constant, `num_days` is a derived
#'                  numeric column, and `count_col` and `norm_col` are columns
#'                  supplied within the input `data` object.
#'
#' @examples
#' set.seed(666)
#' data <- data.frame(disease = "senioritis"
#'  , period_start_date = seq(as.Date("2023-04-03"), as.Date("2023-06-05"), by = 7)
#'  , period_end_date = seq(as.Date("2023-04-09"), as.Date("2023-06-11"), by = 7)
#'  , cases_this_period = sample(0:100, 10, replace = TRUE)
#'  , location = "college"
#' )
#'
#' period_averager(data, keep_raw = TRUE, keep_cols = c("disease", "location"))
#'
#' @family time_periods
#'
#' @importFrom dplyr any_of mutate select
#' @importFrom lubridate year
#' @export
period_averager <- function(data
    , count_col = "cases_this_period"
    , start_col = "period_start_date"
    , end_col = "period_end_date"
    , norm_col = NULL
    , norm_const = 1e5
    , keep_raw = TRUE
    , keep_cols = names(data)
  ){
  if (keep_raw) {
    keep_cols = union(c(start_col, end_col, norm_col, count_col), keep_cols)
  }

  normalize = length(norm_col) == 1L

  data = mutate(data
    , year = lubridate::year(.data[[start_col]])
    , num_days = num_days(.data[[start_col]], .data[[end_col]])
    , period_mid_date = mid_dates(.data[[start_col]], period_length = num_days)
    , period_mid_time = mid_times(.data[[start_col]], period_length = num_days)
    , daily_rate = as.numeric(.data[[count_col]]) / as.numeric(num_days)
  )
  if (normalize) {
    data = mutate(data, daily_rate = daily_rate / .data[[norm_col]])
    data$daily_rate = norm_const * data$daily_rate
  }
  select(data
     , any_of({{keep_cols}})
     , year
     , num_days
     , period_mid_time
     , period_mid_date
     , daily_rate
  )
}

#' Numbers of Days
#'
#' Compute a vector giving the number of days in a
#' set of periods, given equal length vectors of the start date and end date of
#' these periods. This
#'
#' @param start_date Vector of period starting dates
#' @param end_date Vector of period ending dates
#'
#' @concept periods
#' @export
num_days = function(start_date, end_date) num_days_util(start_date, end_date)

#' @importFrom lubridate as_date
#' @describeIn num_days Low-level interface for `num_days`.
#' @export
num_days_util = function(start_date, end_date) {
  as.integer(round(difftime(as_date(end_date), as_date(start_date), units = "days"))) + 1L
}

#' Period Mid-Dates and Mid-Times
#'
#' Compute a vector giving the mid-points of a vector of temporal periods,
#' defined by start dates and one of either a vector of end dates or a vector
#' of period lengths in days (see \code{\link{num_days}}). You can either
#' return a date, with \code{mid_dates}, or a date-time, with \code{mid_times}.
#' In addition to the type of return value (date vs time), the former rounds
#' down to the nearest date whereas the latter is accurate to the nearest hour
#' and so can account for uneven
#'
#' @param start_date Vector of period starting dates
#' @param end_date Vector of period ending dates. If missing then
#' \code{period_length} is used to define the ends of the periods.
#' @param period_length Vector of integers giving the period length in days.
#' If missing then it is calculated using \code{\link{num_days}}.
#'
#' @concept periods
#' @importFrom lubridate as_date as_datetime days hours
#' @name mid_dates_times

#' @rdname mid_dates_times
#' @export
mid_dates = function(start_date, end_date, period_length) {
  mid_util(start_date, end_date, period_length)$dates()
}

#' @rdname mid_dates_times
#' @export
mid_times = function(start_date, end_date, period_length) {
  mid_util(start_date, end_date, period_length)$times()
}

mid_util = function(start_date, end_date, period_length) {
  if (missing(end_date) & missing(period_length)) {
    stop("Please supply either end_date or period_length")
  }
  if (missing(period_length)) period_length = num_days(start_date, end_date)
  dates = function() {
    as_date(start_date) + days(floor(period_length / 2))
  }
  times = function() {
    as_datetime(start_date) + days(floor(period_length / 2)) + hours((period_length %% 2) * 12)
  }
  environment()
}

## data : list with two or three equal-length vectors containing at most
## start_date, end_date, period_length
## (do not do any assumption checking in this utility)
describe_periods_util = function(args, mid_types = c("date", "time")) {
  data = args = as.list(args)
  inputs = names(args)
  missing_start_date = setequal(inputs, c("end_date", "period_length"))
  missing_period_length = setequal(inputs, c("start_date", "end_date"))
  if (missing_start_date) {
    data$start_date = as_date(args$end_date) - days(data$period_length - 1L)
    args$start_date = data$start_date
  }
  if (missing_period_length) {
    data$period_length = num_days(args$start_date, args$end_date)
    args$period_length = data$period_length
  }
  if ("date" %in% mid_types) data$mid_date = do.call(mid_dates, args)
  if ("time" %in% mid_types) data$mid_time = do.call(mid_times, args)
  return(data)
}
