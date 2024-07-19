time_scale_order = c("wk", "2wk", "mo", "qr", "yr")


#' Clean CANMOD CDI Data
#'
#' The important cleaning steps include (1) removing `CA-` from ISO-3166-2
#' codes (because within Canada this is redundant) and (2) filtering out
#' all time-scales but the 'best'.
#' so that there is no chance of double-counting cases.
#'
#' @param canmod_cdi Dataset from IIDDA of type `CANMOD CDI`.
#' @param ... Arguments to pass on to.
#' @importFrom dplyr select mutate filter
#' @export
clean_canmod_cdi = function(canmod_cdi, ...) {
  (canmod_cdi
    |> dplyr::mutate(iso_3166_2 = sub("CA-", "", iso_3166_2))
    |> dplyr::filter(!is.na(iso_3166_2))
    |> iidda::filter_out_time_scales(...)
    |> dplyr::select(-population_reporting)
  )
}

canmod_cdi_only_prov = function(canmod_cdi) {
  (canmod_cdi
    |> dplyr::mutate(iso_3166_2 = sub("CA-", "", iso_3166_2))
    |> dplyr::filter(!iidda::is_empty(iso_3166_2))
    |> dplyr::select(-population_reporting)
  )
}

canmod_cdi_geog_ord = function(canmod_cdi) {
  geog_order = c(
    ## east
      "CA-NL"
    , "CA-PE"
    , "CA-NS"
    , "CA-NB"
    ## central
    , "CA-QC"
    , "CA-ON"
    ## west
    , "CA-MB"
    , "CA-SK"
    , "CA-AB"
    , "CA-BC"
    ## north
    , "CA-YT"
    , "CA-NT"
    , "CA-NU"
  ) |> sub(pattern = "CA-", replacement = "") |> rev()
  dplyr::mutate(canmod_cdi
    , iso_3166_2 = factor(iso_3166_2, levels = geog_order)
  )
}

get_start_end_date = function(n, x) {
  ## Get time period just before the gap
  left = strsplit(x[n], " to ")[[1]][2]
  ## Get time period immediately at the end of the gap
  right = strsplit(x[n+1], " to ")[[1]][1]
  ## Sequence of dates betwen the gaps inclusing of left and right
  x = as.Date(as.Date(left):as.Date(right), origin="1970-01-01")
  if (length(x)==1){
    ## Start and end dates of the gaps, exclusing of left and right
    x = data.frame(period_start_date=x, period_end_date=x)
  } else {
    x = data.frame(period_start_date=x[2], period_end_date=x[(length(x)-1)])
  }
  return(x)
}


split_gaps = function(x) {
  x = (
    x
    |> iidda::summarise_periods(cutoff = Inf)
    ## time periods with gaps
    |> stringr::str_subset(pattern = "\\, ")
    |> strsplit(split = "\\, ")
  )
  if (length(x)) {
    z = x[[1]]
    z = lapply(1:(length(z)-1), get_start_end_date, x=z)
  } else {
    z = data.frame(period_start_date = Date(), period_end_date = Date())
  }
  dplyr::bind_rows(z)
}

get_non_report_dates = function(data, grouping_variable = "time_scale") {
  (data
    |> group_by_at(grouping_variable)
    |> group_modify(~split_gaps(.x))
    |> mutate(days_this_period = num_days(period_start_date, period_end_date)
        , period_mid_time = iidda.analysis::mid_times(period_start_date, period_end_date, days_this_period)
        , cases_this_period = NA_real_
      )
  )
}

#' Data for a Particular Disease
#'
#' @param canmod_cdi Dataset from IIDDA of type `CANMOD CDI`.
#' @param disease_name Name to match in the `nesting_disease` column of a
#' `CANMOD CDI` dataset.
#' @param years If not `NULL`, a vector of years to keep in the output data.
#' @param add_gaps If `TRUE`, add records with `NA` in `cases_this_period`
#' that correspond to time-periods without any data.
generate_disease_df = function(canmod_cdi, disease_name, years = NULL, add_gaps = TRUE) {
  disease_df = (canmod_cdi
    |> filter(
        nesting_disease == disease_name
      , iso_3166_2 != "" ## get province-level data
    )
    |> summarise(
          cases_this_period = sum(cases_this_period)
        , n_sub_diseases = n()
        , any_duplicated_diseases = anyDuplicated(disease)
        , .by = c( ## time x location x disease
            ## time
            period_start_date, period_end_date, time_scale, days_this_period

            ## location{}
          , iso_3166, iso_3166_2, population

            ## disease
          , nesting_disease
        )
    )
    |> mutate(disease = nesting_disease)
  )

  disease_df_rates = (disease_df
    |> mutate(daily_rate = cases_this_period / population / days_this_period)
    |> mutate(period_mid_time = iidda.analysis::mid_times(period_start_date, period_end_date, days_this_period))
    |> mutate(year = lubridate::year(period_mid_time))
  )
  if (!is.null(years)) {
    disease_df_rates = filter(disease_df_rates, year %in% as.character(years))
  }

  ## data frame with records corresponding to the best time scales
  ## for each year-location-disease combination
  if (add_gaps & (nrow(disease_df_rates) > 1L)) {
    ## data frame with records corresponding to gaps in the time-series
    gaps_df = get_non_report_dates(
        disease_df_rates
      , c("time_scale", "disease", "nesting_disease", "iso_3166", "iso_3166_2")
    )
    gaps_df_nation_level = get_non_report_dates(disease_df_rates)
    gaps_df_nation_level$iso_3166 = "CA"
    gaps_df_nation_level$iso_3166_2 = "ON"  # choice doesn't matter
    gaps_df_nation_level$disease = disease_df$disease[1L]
    gaps_df_nation_level$nesting_disease = disease_df$nesting_disease[1L]
    prep_df = bind_rows(disease_df_rates, gaps_df, gaps_df_nation_level)
  } else {
    prep_df = disease_df_rates
  }
  return(prep_df)
}


make_lineplot_df = function(prep_df, time_scale_order, norm_exponent) {
  (prep_df
    ## Aggregate data with time periods
    |> summarise(
        population = sum(population)
      , cases_this_period = sum(cases_this_period)
      , .by = c(period_mid_time, days_this_period, time_scale, year)
    )
    |> mutate(
        daily_rate = (10^norm_exponent * cases_this_period/days_this_period/population)
    )
    |> mutate(time_scale = factor(as.character(time_scale), levels = time_scale_order))
  )
}
