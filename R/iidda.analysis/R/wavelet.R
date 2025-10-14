# PrepWavelet = function() function(data
#     ,
# ) {
#
# }

#' @export
iidda_prep_wavelet = function(data
    , trend_data
    , time_variable = NULL # guess_time(list(data, trend_data))
    , series_variable = NULL
    , trend_variable = NULL
    , series_suffix = "_series"
    , trend_suffix = "_trend"
    , wavelet_variable = "detrend_norm"
    , output_emd_trend = "emd_trend"
    , output_norm = "norm"
    , output_sqrt_norm = "sqrt_norm"
    , output_log_norm = "log_norm"
    , output_emd_norm = "emd_norm"
    , output_emd_sqrt = "emd_sqrt"
    , output_emd_log = "emd_log"
    , output_detrend_norm = "detrend_norm"
    , output_detrend_sqrt = "detrend_sqrt"
    , output_detrend_log = "detrend_log"
    , data_harmonizer = SeriesHarmonizer()
    , trend_data_harmonizer = SeriesHarmonizer()
    , data_deheaper = Deheaper()
    , trend_deheaper = Deheaper()
    , joiner = WaveletJoiner(series_suffix, trend_suffix)
    , interpolator = WaveletInterpolator(series_suffix, trend_suffix)
    , normalizer = WaveletNormalizer(series_suffix,
       trend_suffix,
       output_emd_trend,
       output_norm,
       output_sqrt_norm,
       output_log_norm,
       output_emd_norm,
       output_emd_sqrt,
       output_emd_log,
       output_detrend_norm,
       output_detrend_sqrt,
       output_detrend_log)
    , transformer = WaveletTransformer(dt = 1/52,
        dj = 1/50,
        lowerPeriod = 1/2,
        upperPeriod = 10,
        n.sim=1000,
        make.pval = TRUE,
        date.format = "%Y-%m-%d")
  ) {
  series_harmonized = data_harmonizer(data, time_variable, series_variable)
  trend_harmonized = trend_data_harmonizer(trend_data, time_variable, trend_variable)
  series_deheaped = data_deheaper(series_harmonized, time_variable, series_variable)
  trend_deheaped = trend_deheaper(trend_harmonized, time_variable, trend_variable)
  joined_data = joiner(series_deheaped, trend_deheaped)
  interpolated_data = interpolator(joined_data)
  normalized_data = normalizer(interpolated_data)
  transformed_data = transformer(normalized_data)

  ## Save contour data for plotting, this needs to be saved separately from
  ## data plotting with geom_tile because geom_contour requires a regular grid
  cont_data_to_plot = (
    expand.grid(
      # order of expand.grid matters!
      y_loc = transformed_data$axis.2,
      x_loc = as_datetime(ymd(transformed_data$series$date))
      # can't use Period, creates discrete
    )
    |> mutate(z = c(transformed_data$Power))
    |> mutate(cont = c(transformed_data$Power.pval))
    |> mutate(ridge = c(transformed_data$Ridge * transformed_data$Power))
  )

  ## Compute tile height and width for geom_tile
  tile_data_to_plot = (cont_data_to_plot
    |> arrange(y_loc,x_loc)
    |> mutate(x_wid = as.numeric(difftime(lead(x_loc), x_loc, units = 'hours')))
    |> mutate(x_loc = x_loc + dhours(0.5 * x_wid))
    |> mutate(x_wid = dhours(x_wid))
    |> arrange(x_loc,y_loc)
    |> mutate(y_ht = lead(y_loc)-y_loc)
    |> mutate(y_ht = if_else(y_ht <= 0, min(y_ht[y_ht>0],na.rm=TRUE), y_ht))
    |> mutate(y_ht = if_else(is.na(y_ht),min(y_ht[y_ht>0],na.rm=TRUE), y_ht))
    |> mutate(y_loc = y_loc + (0.5*y_ht))
    |> filter(x_wid > 0)
  )

  return(nlist(transformed_data, tile_data_to_plot, cont_data_to_plot))
}
