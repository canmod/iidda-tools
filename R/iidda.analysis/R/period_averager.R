#' Obtain period midpoints and average daily rates for count data
#'
#' @param data Data frame with rows at minimum containing period start and end
#' dates and a count variable.
#' @param count_col Character, name of count data column.
#' @param start_col Character, name of start date column.
#' @param end_col Character, name of end date column.
#' @param count_name Character, name of column specifying counted variable.
#' @param keep_raw Logical, retain *_col columns in output.
#' @param keep_cols Character vector, names of coluimns to retain in output.
#' To get all columns set \code{keep_cols} to \code{NULL}.
#'
#' @return Data frame containing period length (\code{num_days}),
#' period mid-date (\code{period_mid_date}),
#' average daily count (\code{daily_rate}), and any additional data from `data`.
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
#' @export
period_averager <- function(data
    , count_col = "cases_this_period"
    , start_col = "period_start_date"
    , end_col = "period_end_date"
    , count_name = NULL
    , keep_raw = TRUE
    , keep_cols = c(count_name)
  ){
  if (is.null(keep_cols)) keep_cols = names(data)
  if (keep_raw){keep_cols <- c(keep_cols
      , start_col
      , end_col
      , count_col
    )
  }
  data %>%
  mutate(year = lubridate::year(.data[[start_col]])
     , num_days = num_days(.data[[start_col]], .data[[end_col]]),
     , period_mid_date = mid_dates(data[[start_col]], period_length = num_days)
     , period_mid_time = mid_times(data[[start_col]], period_length = num_days)
     , daily_rate = as.numeric(.data[[count_col]])/as.numeric(num_days)
  ) %>%
    select(any_of({{keep_cols}})

           , year
           , num_days
           , period_mid_date
           , daily_rate )
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
#' @family time_periods
#'
#' @export

num_days_util = function(start_date, end_date) {
  as.integer(round(difftime(end_date, start_date, units = "days"))) + 1L
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
#' @family time_periods
#' @importFrom lubridate as_datetime days hours
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
    as.Date(start_date) + days(period_length %% 2)
  }
  times = function() {
    as_datetime(start_date) + days(period_length %/% 2) + hours(12 * period_length %% 2)
  }
  environment()
  #as.Date(.data[[start_col]]) + lubridate::seconds(period_length * as.numeric(lubridate::days(1))/2)
  #as.Date(start_date) + lubridate::days(period_length)
}
