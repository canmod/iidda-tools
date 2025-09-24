#' Plot a time series
#'
#' Prepare a data set for plotting (trimming, handling missing/zero values, converting the time
#' variable), build a line chart, and apply title, subtitle, and theme.
#'
#' @param data A data frame containing the time-series data (typically output from `iidda_prep_series()`).
#' @param series_variable Name of the series column in `data` (e.g., `"deaths"`).
#' @param time_variable Name of the time column in `data` (e.g., `"period_end_date"`).
#' @param trim_series A `TrimSeries(...)` specification controlling removal of leading/trailing zeros.
#'   Default: `TrimSeries(zero_lead = FALSE, zero_trail = FALSE)`.
#' @param handle_missing_values A `HandleMissingValues(...)` specification controlling NA handling.
#'   Default: `HandleMissingValues(na_remove = FALSE, na_replace = NULL)`.
#' @param handle_zero_values A `HandleZeroValues(...)` specification controlling zero handling.
#'   Default: `HandleZeroValues(zero_remove = FALSE, zero_replace = NULL)`.
#' @param time_variable_converter A `TimeVariableConverter(...)` specifying how to parse/convert the
#'   time variable. Default: `TimeVariableConverter()`.
#' @param compute_moving_average function to compute the moving average of `series_variable`
#' @inheritParams iidda_render_plot
#'
#' @importFrom ggplot2 ggplot geom_line aes scale_x_date scale_y_continuous
#' @concept plotting_functions
#'
#' @return A `ggplot2` plot object containing the time-series line chart with title, subtitle, and theme applied.
#' @export
iidda_series = function(data
    , series_variable = NULL
    , time_variable = NULL
    , trim_series = TrimSeries(zero_lead = FALSE, zero_trail = FALSE)
    , handle_missing_values = HandleMissingValues(na_remove = FALSE, na_replace = NULL)
    , handle_zero_values = HandleZeroValues(zero_remove = FALSE, zero_replace = NULL)
    , time_variable_converter = TimeVariableConverter()
    , title = TitleGuesser()
    , subtitle = TimeRangeDescriber()
    , theme = iidda_theme
) {
  (data
    |> iidda_prep_series(
        series_variable
      , time_variable
      , trim_series
      , handle_missing_values
      , handle_zero_values
      , time_variable_converter
    )
    |> iidda_attach_series(ggplot()
      , series_variable
      , time_variable
    )
    |> iidda_render_plot(
        title
      , subtitle
      , theme
    )
  )
}


AttachSeries = function() function(data
    , initial_ggplot_object = ggplot()
    , series_variable = NULL
    , time_variable = NULL
  ){
  data = resolve_var_args(data)
  plot = (initial_ggplot_object
     + geom_line(
           data = data
         , aes(
            x = .data[[time_variable]]
          , y = .data[[series_variable]]
          )
        )
     + scale_x_date(name = make_axis_title(time_variable), expand = c(0, 0))
     + scale_y_continuous(name = make_axis_title(series_variable), expand = c(0, 0))
  )
  iidda_defaults_if(data, plot = plot)
}

PrepSeries = function() function(data
    , series_variable = NULL
    , time_variable = NULL
    , trim_series = TrimSeries(zero_lead = FALSE, zero_trail = FALSE)
    , handle_missing_values = HandleMissingValues(na_remove = FALSE, na_replace = NULL)
    , handle_zero_values = HandleZeroValues(zero_remove = FALSE, zero_replace = NULL)
    , time_variable_converter = TimeVariableConverter()
  ) {
  data = resolve_var_args(data)
  (data
    |> handle_missing_values()
    |> handle_zero_values()
    |> trim_series()
    |> time_variable_converter()
  )
}

PrepMA <- function() function(data
     , series_variable = NULL
     , time_variable = NULL
     , trim_series = TrimSeries(zero_lead = FALSE, zero_trail = FALSE)
     , handle_missing_values = HandleMissingValues(na_remove = FALSE, na_replace = NULL)
     , handle_zero_values = HandleZeroValues(zero_remove = FALSE, zero_replace = NULL)
     , compute_moving_average = ComputeMovingAverage(ma_window_length = 52)
     , time_variable_converter = TimeVariableConverter()
  ){
  data = resolve_var_args(data)
  (data
    |> handle_missing_values()
    |> handle_zero_values()
    |> trim_series()
    |> time_variable_converter()
    |> compute_moving_average()
  )
}

#' @describeIn iidda_series Attach a time-series plot to a data frame
#' containing the plotted data.
#' @export
iidda_attach_series = AttachSeries()

#' @describeIn iidda_series Prepare a dataset so that it can be used to
#' produce a time-series plot of a moving average.
#' @export
iidda_prep_ma = PrepMA()

#' @describeIn iidda_series Prepare a dataset so that it can be used to
#' produce a time-series plot.
#' @export
iidda_prep_series = PrepSeries()
