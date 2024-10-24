#' Time Series Islands
#'
#' Find 'island rows' in a dataset with ordered rows. Islands have a
#' series variable that is not `NA` surrounded by `NA` values in that
#' same variable. This function could work well with \code{\link{pad_weeks}},
#' if you are looking for weekly 'islands'.
#'
#' @param data A dataset (must be ordered if `time_variable` is `NULL`).
#' @param series_variable Name of a series variable.
#' @param time_variable Optional variable to use for ordering the dataset
#' before islands are located.
#'
#' @importFrom dplyr arrange mutate lag lead filter
#' @export
time_series_islands = function(data, series_variable, time_variable = NULL) {
  if (!is.null(time_variable)) data = arrange(data, get(time_variable))
  data = dplyr::filter(data,
    is.na(dplyr::lag(get(series_variable))) & is.na(dplyr::lead(get(series_variable)))
  )
  dplyr::filter(data, !is.na(get(series_variable)))
}

#' Pad Weeks
#'
#' Add rows to a data frame with `cases_this_period` and `period_end_date` for
#' representing missing weeks. TODO: generalize to other time scales.
#'
#' Could use \url{https://github.com/EdwinTh/padr}.
#'
#' @param data Data frame with a cases_this_period columns and a
#' period_end_date column that is spaced weekly (but possibly with gaps).
#' @param ... Passed on to data.frame to create new constant columns.
#'
#' @returns The input `data` but with new rows for missing weeks. These
#' rows have `NA` in `cases_this_period` and other columns that are not
#' passed through `...` or that were not constant in the input `data` (in which
#' case these constant values are passed on to the output data frame).
#'
#' @importFrom dplyr bind_rows anti_join arrange
#'
#' @export
pad_weeks = function(data, ...) {
  dates = seq(
      from = min(data$period_end_date)
    , to = max(data$period_end_date)
    , by = "week"
  )
  const_cols = names(data)[vapply(data, \(x) 1L == length(unique(x)), logical(1L))]
  all_weeks = data.frame(
      period_end_date = dates
    , period_start_date = dates - lubridate::weeks(1) + lubridate::days(1)
    , cases_this_period = NA_real_
    , ...
  )
  const_cols = setdiff(const_cols, names(all_weeks))
  for (cc in const_cols) all_weeks[[cc]] = unique(data[[cc]])
  padding = anti_join(all_weeks, data, by = "period_end_date")
  bind_rows(data, padding) |> arrange(period_end_date)
}
