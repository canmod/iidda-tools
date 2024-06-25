#' Time Series Islands
#'
#' Find 'island rows' in a dataset with ordered rows. Islands have a
#' series variable that is not `NA` surrounded by `NA` values in that
#' same variable.
#'
#' @param data A dataset (must be ordered if `time_variable` is `NULL`).
#' @param series_variable Name of a series variable.
#' @param time_variable Optional variable to use for ordering the dataset
#' before islands are located.
#'
#' @importFrom dplyr arrange mutate
#' @export
time_series_islands = function(data, series_variable, time_variable = NULL) {
  if (!is.null(time_variable)) data = arrange(data, get(time_variable))
  filter(data,
    is.na(lag(get(series_variable))) & is.na(lead(get(series_variable)))
  )
}
