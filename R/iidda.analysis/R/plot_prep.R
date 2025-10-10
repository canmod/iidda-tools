## Functions that take a data set and return a data set

## Data prep constructors
## ---------------------------------------------------

#' Data Prep Constructors
#'
#' @concept data_prep_constructors
#' @name data_prep_constructors
NULL

#' Skip Pipeline Step
#' @concept data_prep_constructors
#' @export
Skipper = function() function(data, ...) return(data)

DegenerateChooser = function(choice) {
  force(choice)
  function(data, ...) {
    return(choice)
  }
}


#' Custom Data Prep Function
#'
#' Convert a `data_prep_function`, into
#' a function that can be used in an `iidda` data prep pipeline.
#'
#' @param data_prep_function A standard `R` function that takes a data frame,
#' called `data` as its first argument, and returns another data frame.
#' Optionally, subsequent arguments can be added that give the names
#' of types of variables (e.g., `period_end_variable = "period_end_date"`).
#' Consider using `NULL` as the default of these variable name arguments,
#' which will lead to good guesses about variable names when `data` are
#' obtained from `iidda.api`.
#'
#' @returns description A data prep function that maintains
#' `iidda` attributes and guesses at names of types of variables that the
#' `data_prep_function` assumes.
#'
#' @export
CustomDataPrep = function(data_prep_function
    , ign_variables = character(0L)
    , opt_variables = character(0L)
    , new_variables = character(0L)
  ) {
  iidda_data_prep_function = function(data) {
    data = resolve_var_args(data
      , ign_variables = ign_variables
      , opt_variables = opt_variables
      , new_variables = new_variables
    )
    output_data = data_prep_function(data)
    return_iidda(output_data, data)
  }
  formals(iidda_data_prep_function) = formals(data_prep_function)
  return_data_prep_function(iidda_data_prep_function)
}



#' Handle Missing Values in Series Variable
#'
#' Construct a function that takes a data frame and returns another data
#' frame with \code{NA} values either removed or replaced.
#'
#' @param na_remove boolean value, if `TRUE` remove `NA`s in series variable
#' @param na_replace numeric value to replace `NA`s in series variable, if NULL
#' no replacement is performed
#'
#' @importFrom dplyr all_of filter across
#' @return A function like \code{\link{handle_missing_values_default}} that
#' removes or replaces missing values.
#'
#' @concept data_prep_constructors
#' @export
HandleMissingValues <- function(na_remove = FALSE, na_replace = NULL) {
  if (na_remove & !is.null(na_replace)) {
    stop("Cannot both remove and replace missing values.")
  }
  new_variables = character(0L)
  flush_arg_guesses = TRUE

  function(data, series_variable = NULL) {
    data = resolve_var_args(data)

    # remove NAs
    if (na_remove) {
      is_missing = is.na(data[[series_variable]])
      data = data[!is_missing, , drop = FALSE]
    }

    # replace NAs
    is_missing = is.na(data[[series_variable]])
    if (!is.null(na_replace) & any(is_missing)) {
      data[[series_variable]][is_missing] = na_replace
    }

    return(data)
  } ## |> return_data_prep_function()
}

#' @rdname data_prep_default
#' @export
handle_missing_values_default = HandleMissingValues()


#' Handle Zero Values in Series Variable
#'
#' Construct a function that takes a data frame and returns another data
#' frame with \code{0} values either removed or replaced.
#'
#' @param zero_remove boolean value, if `TRUE` remove zeroes in series variable
#' @param zero_replace numeric value to replace zeroes in series variable, if NULL
#' no replacement is performed
#'
#' @returns A function like \code{\link{handle_zero_values_default}} to remove
#' or replace zero values.
#'
#' @concept data_prep_constructors
#' @export
HandleZeroValues <- function(zero_remove = FALSE, zero_replace = NULL){
  if (zero_remove & !is.null(zero_replace)) {
    stop("Cannot both remove and replace zeros.")
  }
  new_variables = character(0L)
  flush_arg_guesses = TRUE
  function(data, series_variable = NULL) {
    data = resolve_var_args(data)

    # remove zeros
    if (zero_remove) {
      is_zero = Vectorize(isTRUE)(as.integer(round(data[[series_variable]])) == 0L)
      data = data[!is_zero, , drop = FALSE]
    }

    # replace zeroes
    is_zero = Vectorize(isTRUE)(as.integer(round(data[[series_variable]])) == 0L)
    if (!is.null(zero_replace) & any(is_zero)) {
      data[[series_variable]][is_zero] = zero_replace
    }
    return(data)
  } ## |> return_data_prep_function()
}


#' @rdname data_prep_default
#' @export
handle_zero_values_default = HandleZeroValues()



#' Trim Time Series
#'
#' Remove leading or trailing zeros in a time series data set.
#'
#' @param zero_lead boolean value, if `TRUE` remove leading zeroes in `data`
#' @param zero_trail boolean value, if `TRUE` remove trailing zeroes in `data`
#'
#' @importFrom dplyr arrange pull
#' @returns A function like \code{\link{trim_series_default}} to remove to
#' remove leading and/or trailing zeroes.
#'
#' @concept data_prep_constructors
#' @export
TrimSeries <- function(zero_lead = FALSE, zero_trail = FALSE) {
  new_variables = character(0L)
  flush_arg_guesses = TRUE
  function(data, series_variable = NULL, time_variable = NULL) {
    data = resolve_var_args(data)
    # trim leading zeroes
    if (zero_lead) {
      first_date <- (data
         |> filter(get(series_variable) != 0)
         |> arrange(get(time_variable))
         |> filter(row_number() == 1)
         |> select(all_of(time_variable))
         |> pull()
      )
      data <- filter(data, get(time_variable) >= first_date)
    }

    # trim trailing zeroes
    if (zero_trail) {
      last_date <- (data
        |> filter(get(series_variable) != 0)
        |> arrange(get(time_variable))
        |> filter(row_number() == n())
        |> select(all_of(time_variable))
        |> pull()
      )
      data <- filter(data, get(time_variable) <= last_date)
    }
    return(data)
  } ## |> return_data_prep_function()
}

#' @rdname data_prep_default
#' @export
trim_series_default = TrimSeries()


#' Series Harmonizer
#'
#' Harmonizes the series variable in `data` so there is one data value for each time
#' unit in time variable (to account for different variations in disease/cause name)
#'
#' @param time_variable column name of time variable in `data`, default is "period_end_date"
#' @param series_variable column name of series variable in `data`, default is "deaths"
#'
#' @importFrom dplyr group_by summarize ungroup
#' @returns A function like \code{\link{series_harmonizer_default}} to 
#' harmonize disease/cause names.
#'
#' @concept data_prep_constructors
#' @export
SeriesHarmonizer = function(sum_fn = base::sum) {
  flush_arg_guesses = TRUE
  function(data, series_variable = NULL, time_variable = NULL) {
    data = resolve_var_args(data)
    harmonized_data = (data
     |> group_by(across(all_of(time_variable)))
     |> summarize(!!sym(series_variable) := sum_fn(get(series_variable)))
     |> ungroup()
    )

    ## necessary because ungroup removes attributes
    return_iidda(harmonized_data, data)
  } ## |> return_data_prep_function()
}

#' @rdname data_prep_default
#' @export
series_harmonizer_default = SeriesHarmonizer()

#' De-heaping time series
#'
#' Fixes heaping errors in time series. The structure of this function was taken from
#' the function `find_heap_and_deheap` created by Kevin Zhao
#' (https://github.com/davidearn/KevinZhao/blob/main/Report/make_SF_RData.R). This needs
#' to be better documented.
#'
#' @param first_date string containing earliest date to look for heaping errors
#' @param last_date string containing last date to look for heaping errors
#' @param week_start numeric value of the first week number to start looking for heaping errors
#' @param week_end numeric value of the last week number to look for heaping errors
#'
#' @importFrom dplyr if_else group_split lag lead
#' @importFrom stats sd
#' @return A function like \code{\link{deheaper_default}} to fix heaping errors.
#'
#' @importFrom stats median start end
#' @concept data_prep_constructors
#' @export
Deheaper = function(
      prefix = "deheaped_"
    , first_date = "1830-01-01"
    , last_date = "1841-12-31"
    , week_start = 45
    , week_end = 5
    , deheaping_scale = 2.7
) {
  new_variables = character(0L)


  ## year_data : data frame with data for a single year
  ## return : ??
  year_level_deheap = function(series_variable, time_variable, deheaped_variable) function(year_data) {

    ## updated_series+((old-new)*updated_series)/sum(updated_series)
    ## utilities for table transformations

    mk_do_heap = function(series_variable, med, stdev) {
      series_variable - med >= deheaping_scale * stdev
    }
    deheap_prep = function(series_variable, do_heap) {
      if_else(do_heap
        , (lag(series_variable) + lead(series_variable))/2
        , series_variable
      )
    }

    # identify potential heap value and compute updated
    # value by averaging previous and post data
    intermediate_data <- (year_data
      |> mutate(
          stdev = sd(get(series_variable))
        , med = median(get(series_variable))
      )
      # this heaping threshold was taking
      # directly from `find_heap_and_deheap`
      |> mutate(do_heap = mk_do_heap(get(series_variable), med, stdev))
      |> arrange(get(time_variable))
      |> mutate(updated_series = deheap_prep(get(series_variable), do_heap))
    )
    # get old value of heaped data point
    old <- intermediate_data[[series_variable]][intermediate_data$do_heap]

    deheap = function(x) x + (((old - new) * x) / sum(x))
    if (length(old) == 1) {

      # get new value of heaped data point
      new <- intermediate_data$updated_series[intermediate_data$do_heap]

      # compute deheaped series
      intermediate_data <- (intermediate_data
        |> mutate(!!sym(deheaped_variable) := deheap(updated_series))
        |> select(all_of(time_variable), all_of(deheaped_variable))
      )
    } else {
      # set deheaped series to current series
      # TODO Should find a way to return no deheaped series if it is not needed
      intermediate_data <- (intermediate_data
        |> mutate(!!sym(deheaped_variable) := get(series_variable))
        |> select(all_of(time_variable), all_of(deheaped_variable))
      )
    }
    return(intermediate_data)
  }
  flush_arg_guesses = TRUE
  ## output function
  function(data, series_variable = NULL, time_variable = NULL) {
    data = resolve_var_args(data)

    mk_year_groups = function(time_variable) {
      if_else(
          week(time_variable) >= week_start
        , year(time_variable)
        , year(time_variable) - 1
      )
    }
    deheaped_variable = paste0(prefix, series_variable)
    year_level_deheap = year_level_deheap(series_variable, time_variable, deheaped_variable)


    # this computation to create the deheaped field was
    # taken directly from `find_heap_and_deheap`
    enforce_range = function(time_variable) {
      (time_variable >= first_date) & (time_variable <= last_date)
    }
    core = function(time_variable) {
      week(time_variable) >= week_start | week(time_variable) <= week_end
    }

    # filter data by time period and relevant weeks,
    # group by year while accounting for year-ends
    filter_data <- (data
      |> filter(enforce_range(get(time_variable)))
      |> filter(core(get(time_variable)))
      |> mutate(year_groups = mk_year_groups(get(time_variable)))
      |> group_by(year_groups)
      |> group_split()
    )
    if (length(filter_data) != 0) {
      deheaped_data <- filter_data |> lapply(year_level_deheap) |> bind_rows()
      all_data <- left_join(data, deheaped_data, by = time_variable)
    }

    output = return_iidda(all_data, data)
    attr(output, "iidda")$series_variable = deheaped_variable
    return(output)
  } ## |> return_data_prep_function()
}

#' @rdname data_prep_default
#' @export
deheaper_default = Deheaper()


#' Wavelet Joiner
#'
#' Joins series data and trend datasets and keeps all time units in one of the
#' datasets.
#'
#' @param time_variable column name of time variable in `data`, default is "period_end_date"
#' @param series_suffix suffix to be appended to series data fields
#' @param trend_suffix suffix to be appended to trend data fields
#' @param keep_series_dates boolean flag to indicate if the dates in `series_data` should
#' be kept and data from `trend_data` is left joined, if `FALSE` dates from `trend_data`
#' are left joined instead
#'
#' @returns A function like \code{\link{wavelet_joiner_default}} to join data
#' and trend data sets.
#'
#' @concept data_prep_constructors
#' @noRd
WaveletJoiner = function(
    time_variable = "period_end_date",
    series_suffix = "_series",
    trend_suffix = "_trend",
    keep_series_dates = TRUE
) {
  new_variables = character(0L)
  function(series_data, trend_data) {
    if (keep_series_dates)
    {
      (series_data
       |> left_join(trend_data,by=time_variable, suffix=c(series_suffix, trend_suffix))
      )
    } else {
      (trend_data
       |> left_join(series_data,by=time_variable, suffix=c(trend_suffix, series_suffix))
      )
    }
  } ## |> return_data_prep_function()
}


#' Wavelet Interpolator
#'
#' Linearly interpolates `NA` values in both series and trend variables.
#'
#' @param series_variable column name of series variable in `data`, default is "deaths_series"
#' @param trend_variable column name of series variable in `data`, default is "deaths_trend"
#' @param time_variable column name of time variable in `data`, default is "period_end_date"
#' @param series_suffix suffix to be appended to series data fields
#' @param trend_suffix suffix to be appended to trend data fields
#'
#' @importFrom stats approx
#' @importFrom dplyr union
#' @return function that linearly interpolates series and trend data.
#'
#' ## Returned Function
#'
#' - Arguments
#'    * `data` data frame containing time series data
#' - Return - `data` with linearly interpolated `series_variable` and
#' `trend_variable`
#'
#' @concept data_prep_constructors
#' @noRd
WaveletInterpolator = function(
      series_suffix = "_series"
    , trend_suffix = "_trend"
  ) {
  new_variables = character(0L)
  function(data, time_variable, series_variable, trend_variable) {

    # form field names
    series_variable = paste0(series_variable, series_suffix)
    trend_variable = paste0(trend_variable, trend_suffix)

    # filter out NA values
    series_data <- (data
                    |> filter(!is.na(get(series_variable)))
                    |> select(time_variable, series_variable)
    )
    trend_data <- (data
                   |> filter(!is.na(get(trend_variable)))
                   |> select(time_variable, trend_variable)
    )

    # identify dates with NA values in series and trend variables
    series_dates <- (data
                     |> filter(is.na(get(series_variable)))
                     |> select(all_of(time_variable))
                     |> pull()
    )
    trend_dates <- (data
                    |> filter(is.na(get(trend_variable)))
                    |> select(all_of(time_variable))
                    |> pull()
    )

    # interpolate at time point corresponding to NA values in
    # series and trend variables
    series_interp <- (approx(x=series_data[[time_variable]],
                             y=series_data[[series_variable]],
                             xout=series_dates,
                             method="linear")
                      |> data.frame()
                      |> setNames(c(time_variable,series_variable))
    )

    trend_interp <- (approx(x=trend_data[[time_variable]],
                            y=trend_data[[trend_variable]],
                            xout=trend_dates,
                            method="linear")
                     |> data.frame()
                     |> setNames(c(time_variable,trend_variable))
    )
    # combined final data set
    (data
      |> select(-series_variable, -trend_variable)
      |> left_join((series_data |> union(series_interp)),by=time_variable)
      |> left_join((trend_data |> union(trend_interp)),by=time_variable)
      # remove leading and trailing NAs (that can't be interpolated)
      |> filter(!is.na(get(series_variable)),!is.na(get(trend_variable)))
    )

  } ## |> return_data_prep_function()
}


#' Wavelet Normalizer
#'
#' Creates normalizing fields in `data`
#'
#' @param time_variable column name of time variable in `data`, default is "period_end_date"
#' @param series_variable  column name of series variable in `data`, default is "deaths_series"
#' @param trend_variable column name of series variable in `data`, default is "deaths_trend"
#' @param series_suffix suffix to be appended to series data fields
#' @param trend_suffix suffix to be appended to trend data fields
#' @param output_emd_trend name of output field for the empirical mode decomposition applied to `trend_variable`
#' @param output_norm name of output field for the `series_variable` normalized by `output_emd_trend`
#' @param output_sqrt_norm name of output field for the square root of `output_norm`
#' @param output_log_norm name of output field for the logarithm of (`output_norm` + `eps`)
#' @param output_emd_norm name of output field for the empirical mode decomposition applied to `output_norm`
#' @param output_emd_sqrt name of output field for the empirical mode decomposition applied to `output_sqrt_norm`
#' @param output_emd_log name of output field for the empirical mode decomposition applied to `output_log_norm`
#' @param output_detrend_norm name of output field for the computed field `output_norm`-`output_emd_norm`
#' @param output_detrend_sqrt name of output field for the computed field `output_sqrt_norm`-`output_emd_sqrt`
#' @param output_detrend_log name of output field for the computed field `output_log_norm`-`output_emd_log`
#' @param eps numeric value for normalized data to be perturbed by before
#' computing the logarithm
#'
#' @importFrom EMD emd
#' @return function that creates normalized trend and de-trended fields
#'
#' ## Returned Function
#'
#' - Arguments
#'    * `data` data frame containing time series data
#' - Return - `data` with additional normalized fields
#'
#' @concept data_prep_constructors
#' @noRd
WaveletNormalizer = function(
    series_suffix = "_series",
    trend_suffix = "_trend",
    output_emd_trend = "emd_trend",
    output_norm = "norm",
    output_sqrt_norm = "sqrt_norm",
    output_log_norm = "log_norm",
    output_emd_norm = "emd_norm",
    output_emd_sqrt = "emd_sqrt",
    output_emd_log = "emd_log",
    output_detrend_norm = "detrend_norm",
    output_detrend_sqrt = "detrend_sqrt",
    output_detrend_log = "detrend_log",
    eps = 0.01
) {
  new_variables = character(0L)
  function(data, time_variable, series_variable, trend_variable) {

    # form field names
    series_variable = paste0(series_variable, series_suffix)
    trend_variable = paste0(trend_variable, trend_suffix)


    (data
     |> arrange(get(time_variable))
     # can't seem to use a date variable for tt, using row index instead
     |> mutate(!!output_emd_trend := emd(xt = data[[trend_variable]],
                                tt = row_number(),
                                boundary="wave")$residue,
                !!output_norm := get(series_variable)/get(output_emd_trend),
                !!output_sqrt_norm := sqrt(get(output_norm)),
                !!output_log_norm := log(get(output_norm)+eps),
                !!output_emd_norm := emd(xt=get(output_norm),tt=row_number(),boundary="wave")$residue ,
                !!output_emd_sqrt := emd(xt=get(output_sqrt_norm),tt=row_number(),boundary="wave")$residue ,
                !!output_emd_log := emd(xt=get(output_log_norm),tt=row_number(),boundary="wave")$residue ,
                !!output_detrend_norm := get(output_norm)-get(output_emd_norm),
                !!output_detrend_sqrt := get(output_sqrt_norm)-get(output_emd_sqrt),
                !!output_detrend_log := get(output_log_norm)-get(output_emd_log))
    )
  } ## |> return_data_prep_function()
}

#' Wavelet Transformer
#'
#' Compute the wavelet transform.
#'
#' @param time_variable name of the time variable field in `data`
#' @param wavelet_variable name of the field in `data` to be wavelet transformed
#' @param ... Arguments passed on to \code{\link{analyze.wavelet}}.
#'
#' @importFrom WaveletComp analyze.wavelet
#' @return function that computes wavelet transform
#'
#' ## Returned Function
#'
#' - Arguments
#'    * `data` data frame containing time series data
#' - Return - the wavelet transform object from EMD::analyze.wavelet applied to
#' `wavelet_variable` in `data`
#'
#' @concept data_prep_constructors
#' @noRd
WaveletTransformer = function(...) { # all arguments to analyze.wavelet
  new_variables = character(0L)
  function(data, time_variable, wavelet_variable) {
    analyze_data <- (data |> rename("date" = time_variable))
    analyze.wavelet(my.data = analyze_data,
                    my.series = wavelet_variable,
                    ...
    )
  } ## |> return_data_prep_function()
}



#' Compute Moving Average of Time Series
#'
#' @param ma_window_length length of moving average window, this will depend on the time scale in the data.
#' Defaults to 52, so that weekly data is averaged over years.
#'
#' @returns A function like \code{\link{compute_moving_average_default}} to 
#' remove to compute the moving average of a time series variable.
#'
#' @concept data_prep_constructors
#' @export
ComputeMovingAverage <- function(ma_window_length=52){
  function(data
      , series_variable = NULL
      , time_variable = NULL
    ){
    data = resolve_var_args(data)
    
    # compute moving average
    ma_filter <- rep(1,ma_window_length)/ma_window_length

    data <- data |> mutate(across(all_of(series_variable), ~stats::filter(.,ma_filter)))
    #data$y <- stats::filter(data$y,ma_filter)

    return_iidda(data)
  }
}

#' @rdname data_prep_default
#' @export
compute_moving_average_default = ComputeMovingAverage()

#' Period Aggregator
#'
#' Create function that aggregates information over time periods, normalizes
#' a count variable, and creates new fields to summarize this information.
#'
#' @param rate_variable Name of variable to be used to store the normalized
#' count variable.
#' @param norm_exponent Exponent to use in normalization. The default is `5`,
#' which means `per 100,000`.
#'
#' @returns A function like \code{\link{period_aggregator_default}} that
#' aggregates data so that each time period is represented by exactly 
#' one record.
#'
#' @concept data_prep_constructors
#' @export
PeriodAggregator = function(rate_variable, norm_exponent = 5) {
  function(data                    # Examples
                                   # --------
    , time_variable = NULL         # "period_end_date"
    , period_width_variable = NULL # "num_days"
    , count_variable = NULL        # "cases_this_period"
    , norm_variable = NULL         # "population_reporting"
  ) {
    data = resolve_var_args(data)
    output_data = (data
      |> group_by(.data[[time_variable]], .data[[period_width_variable]])
      |> summarise(
          !!norm_variable := sum(.data[[norm_variable]])
        , !!count_variable := sum(.data[[count_variable]])
      )
      |> ungroup()
      |> mutate(!!rate_variable := (
            10^norm_exponent
          * get(count_variable)
          / get(period_width_variable)
          / get(norm_variable)
      ))
    )
    return_iidda(output_data, data)
  } ## |> return_data_prep_function()
}

#' @rdname data_prep_default
#' @export
period_aggregator_default = PeriodAggregator()

#' Count Aggregator
#' 
#' Create a function that aggregates count variables.
#' 
#' @returns A function like \code{\link{count_aggregator_default}} that
#' aggregates count variables.
#' @concept data_prep_constructors
#' @export
CountAggregator = function() {
  function(data
    , total_count_variable = NULL
    , count_variable = NULL
    , grouping_variable = NULL
  ) {
    data = resolve_var_args(data, ign_variables = "total_count_variable")
    data = resolve_specific_arg(data
      , arg = "total_count_variable"
      , default = sprintf("total_%s", count_variable)
    )
    aggregated = (data
      |> summarise(
            !!sym(total_count_variable) := sum(get(count_variable), na.rm = TRUE)
          , .by = all_of(grouping_variable)
        )
      |> arrange(desc(get(total_count_variable)))
      |> mutate(!!sym(grouping_variable) := factor(get(grouping_variable), levels = get(grouping_variable)))
    )
    return_iidda(data, more_iidda_attrs = list(aggregated = aggregated))
  }## |> return_data_prep_function()
}

#' @rdname data_prep_default
#' @export
count_aggregator_default = CountAggregator()


#' Period Describer
#'
#' Create a function that takes a data set containing at least two of
#' the following variables, `period_start_variable`, `period_end_variable`,
#' `period_days_variable`, and returning a data set with all of these
#' three variables and other variables describing the middle of the period
#' with either or both of `period_mid_time_variable` and
#' `period_mid_date_variable`. These two period middle descriptors will only
#' differ (by exactly 12 hours) for periods with odd numbers of days.
#'
#' @param mid_types Compute mid-times and/or mid-dates?
#'
#' @returns A function like \code{\link{period_describer_default}} that
#' adds variables to describe the time period represented by each record.
#' @concept data_prep_constructors
#' @export
PeriodDescriber = function(mid_types = c("time", "date")) {
  
  ## ---- utility functions for the describer ------
  
  ## get the name map for the user-supplied combination 
  ## of columns that describe periods
  ## (e.g., if period_start_date_variable and period_days_variable are
  ## available then these are the only columns in the name map)
  get_map_input = function(nm_map, nms) {
    i = nm_map %in% nms
    nm_map_input = nm_map[i]
    if (sum(i) < 2L) {
      additional_info = ", but didn't contain any of these."
      if (sum(i) == 1L) {
        additional_info = sprintf(", but only contained %s", nm_map_input)
      }
      msg(
          "Data needs to contain at least two of"
        , paste(nm_map, collapse = ", ")
        , additional_info
      ) |> stop()
    }
    return(nm_map_input)
  }

  add_period_mid_descriptors = function(nm_map
      , mid_types
      , period_mid_time_variable
      , period_mid_date_variable
  ) {
    if ("time" %in% mid_types) {
      nm_map = c(nm_map, mid_time = period_mid_time_variable)
    }
    if ("date" %in% mid_types) {
      nm_map = c(nm_map, mid_date = period_mid_date_variable)
    }
    return(nm_map)
  }

  ## all variables are optional, as long as at least
  ## two of period_start_variable, period_end_variable,
  ## or period_days_variable are specified
  flush_arg_guesses = TRUE
  
  function(data
    , period_start_variable = NULL
    , period_end_variable = NULL
    , period_mid_time_variable = NULL
    , period_mid_date_variable = NULL
    , period_days_variable = NULL
  ) {
    data = resolve_var_args(data
      , opt_variables = c(
            "period_start_variable", "period_end_variable"
          , "period_mid_time_variable", "period_mid_date_variable"
          , "period_days_variable"
        )
    )
    
    ## map names of arguments between
    ## two interfaces -- yes we have
    ## control over both so we will
    ## harmonize them if we have time
    nm_map = c(
        start_date = period_start_variable
      , end_date = period_end_variable
      , period_length = period_days_variable
    )
    
    nm_map_input = get_map_input(nm_map, names(data))
    
    period_info = setNames(
        as.list(data[ , nm_map_input, drop = FALSE])
      , names(nm_map_input)
    )
    period_info = describe_periods_util(period_info, mid_types)[]  ## why [] ??
    
    nm_map = add_period_mid_descriptors(nm_map
      , mid_types
      , period_mid_time_variable
      , period_mid_date_variable
    )
    
    for (var in names(period_info)) data[[nm_map[var]]] = period_info[[var]]
    
    return(data)
  }
}

#' @rdname data_prep_default
#' @export
period_describer_default = PeriodDescriber()

DateDescriber = function(
      year_variable = "year"
    , month_variable = "month"
    , day_variable = "day"
  ) {
  function(data, date_variable = NULL) {
    data = resolve_var_args(data)
    if (is.character(data[[date_variable]])) {
      
    }
  }
}

#' Time Scale Picker
#'
#' @returns A function like \code{\link{time_scale_picker_default}} that 
#' 
#' @export
TimeScalePicker = function() {
  pick_fine_time_scale = function(scales) {
    ordering = c("wk", "mo", "qr", "yr")
    f = factor(as.character(scales), levels = ordering)
    f[which.min(as.numeric(f))] |> as.character()
  }
  new_variables = character(0L)
  flush_arg_guesses = TRUE
  function(data
    , time_scale_variable = NULL # "time_scale"
    , time_group_variable = NULL # "year"
  ) {
    data = resolve_var_args(data)
    if (time_group_variable %in% names(data)) {
      data = group_by(data, .data[[time_group_variable]])
    }
    grouped_data = (data
      # |> group_by(.data[[time_group_variable]])
      |> filter(.data[[time_scale_variable]] == pick_fine_time_scale(.data[[time_scale_variable]]))
      |> ungroup()
    )
    return_iidda(grouped_data, data)
  } ## |> return_data_prep_function()
}

#' Time Variable Converter
#'
#' Construct a function that takes a data frame and returns another data
#' frame with a time variable converted so that it has the correct format,
#' class, and/or type.
#'
#' @param as_date Function that takes a vector and converts it to a date vector if possible. Used only if `time_variable` is in `std_date_variables()`.
#' @param as_integer Function that takes a vector and converts it to an integer vector if possible. Used only if `time_variable` is in `std_integer_variables()`.
#' @param as_numeric Function that takes a vector and converts it to a numeric vector if possible. Used only if `time_variable` is in `std_numeric_variables()`.
#'
#' ## Returned Function
#'
#' - Arguments :
#'     * `data` : Data frame containing a time variable.
#'     * `time_variable` : Column name of time variable in `data`. The default
#'     is `"period_end_date"`.
#' - Return : A version of `data` with `time_variable` column converted to
#'
#' @concept data_prep_constructors
#' @export
TimeVariableConverter = function(
      as_date = as.Date
    , as_integer = as.integer
    , as_numeric = as.numeric
  ) {
  new_variables = character(0L)
  flush_arg_guesses = TRUE
  function(data, time_variable = NULL) {
    data = resolve_var_args(data)
    if (time_variable[1L] %in% std_date_variables()) {
      data[[time_variable]] = as_date(data[[time_variable]])
    } else if (time_variable[1L] %in% std_integer_time_variables()) {
      data[[time_variable]] = as_integer(data[[time_variable]])
    } else if (time_variable[1L] %in% std_numeric_time_variables()) {
      data[[time_variable]] = as_numeric(data[[time_variable]])
    } else {
      sprintf(
        "cannot coerce %s into an appropriate type. please do so manually."
        , time_variable
      ) |> stop()
    }
    return(data)
  } ## |> return_data_prep_function()
}

#' Data Dictionary Converter
#'
#' @export
DataDictionaryConverter = {
  new_variables = character(0L)
  function(data_dictionary = iidda_data_dictionary()) {
    function(data) iidda::parse_columns(data, data_dictionary)
  } ## |> return_data_prep_function()
}

#' Time Range Desciber
#'
#' @concept data_prep_constructors
#' @importFrom iidda summarise_dates
#' @export
TimeRangeDescriber = function(cutoff = 50) {
  function(data, period_start_variable = NULL, period_end_variable = NULL) {
    data = resolve_var_args(data)
    time_range_string = iidda::summarise_periods_vec(
        data[[period_start_variable]]
      , data[[period_end_variable]]
      , cutoff
    )
    return(time_range_string)
  }
}

#' Title Guesser
#'
#' @param custom Custom string for the title
#' @param prefer List of variables that could contain title information
#' in an order that will be used to find variables that will be used to
#' guess at a title. The first variable found in the data is the one that
#' is chosen.
#' @export
TitleGuesser = function(
      custom = NULL
    , prefer = std_title_variables()
  ) {
  assert_custom = function(data, value) {
    if (!is_string(value)) {
      msg(
          "Custom titles must be a single string"
        , "(i.e., length-1 character vector)."
      ) |> stop()
    }
    return(value)
  }
  get_candidates = function(data) {
    candidates = (data
      |> Filter(f = \(x) is.character(x) | is.factor(x))
      |> lapply(unique)
      |> Filter(f = \(x) length(x) == 1L)
      |> lapply(as.character)
      |> Filter(f = \(x) nchar(x) > 0L)
    )
    if (length(candidates) == 0L) {
      msg(
          "The data contain no constant character or factor variables"
        , "that could be used as a title for this plot. Please"
        , "specify a custom_title."
      )
    }
    return(candidates)
  }
  get_from_user_specified_variable = function(variable, candidates) {
    if (!is.null(variable)) {
      if (variable %in% names(candidates)) return(candidates[[variable]])
      msg(
          sprintf("The title variable, %s,", variable)
        , "is either not in the data or is not a constant character"
        , "or factor variable with unique value that could be used as"
        , "a title for this plot."
      ) |> stop()
    }
    return(NULL)
  }
  get_preferred_candidate = function(data, prefer, candidates) {
    if (any(prefer %in% names(candidates))) {
      return(candidates[prefer[prefer %in% names(candidates)]][[1L]])
    }
    return(NULL)
  }
  function(data, title_variable = NULL) {
    if (!is.null(custom)) {
      title = assert_custom(data, custom)
    } else {
      candidates = get_candidates(data)
      title = get_from_user_specified_variable(title_variable, candidates)
      if (is.null(title)) {
        title = get_preferred_candidate(data, prefer, candidates)
        if (is.null(title)) title = candidates[[1L]]
        sprintf(
          "Guessed that %s should be the title of the plot", title
        ) |> message()
      }
    }
    return(title)
  }
}

OrderGuesser = function() {
  function(data, category_variable = NULL) {
    data = resolve_var_args(data)
    data[[category_variable]] |> unique()
  }
}



VariableTitleGuesser = function(dictionary = iidda_data_dictionary()) {
  function(variable) {
    i = dictionary$name == variable
    if (!any(i)) return(variable)
    dictionary$title[[which(i)]]
  }
}

# ------------------------------------
# prep functions TODO: better name?

#' @importFrom iidda parse_columns
#' @export
iidda_set_column_types = function(data) {
  dict = ("data_dictionary.rdata"
    |> system.file(package = "iidda.analysis")
    |> readRDS()
  )
  (data
    |> iidda::parse_columns(dict)
    |> return_iidda(data)
  )
}

#' Union Time Series
#'
#' Combine two time series data sets with the option to handle overlapping time periods.
#' This is particularly useful for data sets that come from two sources (ex. LBoM and RG).
#' Assumes both data sets have the same number of columns with the same names.
#'
#' @param x first data frame containing time series data
#' @param y second data frame containing time series data
#' @param overlap boolean to indicate if `x` should get priority with overlapping time periods in `y`.
#' If `TRUE` the returned data frame will contain all data from `x`, and the filtered `y` data that does
#' not overlap with `x`. If FALSE, a union between `x` and `y` is returned.
#' @param time_variable column name of time variable in `x` and `y`, default is "period_end_date"
#'
#' @importFrom janitor compare_df_cols_same
#' @return combined `x` and `y` data frames with optional filtering for overlaps
#'
#' @export
union_series <- function(x,
                         y,
                         overlap = TRUE,
                         time_variable="period_end_date"){

  # check that they have the same column names, and number of columns
  # should I be using janitor package? ...(minimize number of packages used?)
  stopifnot(janitor::compare_df_cols_same(x,y))

  ## if overlapping is required
  if (overlap){
    ## get time ranges for both data sets
    x_start <- min(x[,time_variable])
    x_end <- max(x[,time_variable])
    y_start <- min(y[,time_variable])
    y_end <- max(y[,time_variable])

    ## if either data set is nested within the other, then return x only
    if ( (y_start > x_start & y_end < x_end) | (y_start < x_start & y_end > x_end)){
      cat("Data sets are nested. Returning x.")
      data <- x
    } ## y starts earlier than x ends, and the data frames aren't nested
      else if (x_end > y_start & x_start < y_start){
      data <- union(x, filter(y,get(time_variable)>x_end))

    }  ## y ends later than x starts, and the data frames aren't nested
      else if (y_end > x_start & y_start < x_start) {
      data <- union(x, filter(y, get(time_variable) < x_start))

    } ## There are no time period overlaps, return the default union
      else {
      cat("There was no overlap between x and y. The output contains all data from x and y.")
      data <- union(x,y)
    }

    ## No overlapping is requested
  } else {
    data <- union(x,y)
  }
 return(data)
}


#' Year End Fix
#'
#' Weeks covering the year end are split into two records. The first week is adjusted to end on day 365 (or 366 in leap years),
#' and the second week starts on the first day of the year. This was adapted from `LBoM::edge_fix` which keeps the same
#' series variable value for both of the newly created weeks. This doesn't seem to make much difference when viewing the
#' seasonal heatmap, however it might make sense to do something sensible like dividing the series variable value in half and allocating
#' each week to have half of the values.
#'
#' @param data data frame containing time series data
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param start_year_variable column name of time variable containing the year of the starting period, defaults to "Year"
#' @param end_year_variable column name of time variable containing the year of the ending period, defaults to "End Year"
#' @param start_day_variable column name of time variable containing the day of the starting period, defaults to "Day of Year"
#' @param end_day_variable column name of time variable containing the day of the ending period, defaults to "End Day of Year"
#' @param temp_year_variable temporary variable name when pivoting the data frame
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr rename
#' @return all fields in `data` with only records corresponding to year end weeks that have been split
#' @export
year_end_fix <- function(data,
                         series_variable="deaths",
                         # should these be the default names? probably should use get(unit_label)
                         # and prepend string to identify start and end?
                         start_year_variable = "Year",
                         end_year_variable = "End Year",
                         start_day_variable = "Day of Year",
                         end_day_variable = "End Day of Year",
                         temp_year_variable = "yr"){
  fixed_data <- (data
                 # isolate year end weeks
                 |> filter(get(start_year_variable)!= get(end_year_variable))
                 |> pivot_longer(cols=c(start_year_variable,end_year_variable),names_to=temp_year_variable,values_to="value")
                 |> mutate(!!end_day_variable := if_else(value %% 4 !=0 & get(temp_year_variable)==start_year_variable, 365, get(end_day_variable)),
                            !!end_day_variable := if_else(value %% 4 ==0 & get(temp_year_variable)==start_year_variable, 366, get(end_day_variable)))
                 |> mutate(!!start_day_variable := if_else(get(temp_year_variable)== end_year_variable, 0, get(start_day_variable)))
                 |> rename(!!start_year_variable:=value)
                 # create temp end year field, is this really needed?
                 |> mutate(!!end_year_variable := get(start_year_variable))
                 |> select(-all_of(temp_year_variable))
  )
}

#' Log1p Scale Transformation
#'
#' Slight modification of `log1p_trans()` to include better breaks that are log1p-based (log-based and
#' shifted 1 so that breaks can be computed in the presence of zeroes.)
#'
#' @param n number of desired breaks
#'
#' @importFrom scales trans_new
#' @importFrom grDevices axisTicks
#' @return a \code{scales::trans_new} function
#' @export
log1p_modified_trans <- function(n=10){
  scales::trans_new("log1p_modified",
                    "log1p",
                    "expm1",
                    breaks = function(x) {
                      # x + 1 so we avoid the issue of zeroes
                      axisTicks(log(range(x+1, na.rm = TRUE)), log = TRUE, nint = n)
                    })
}

#' Quantile Transformation
#'
#' Quantile transformation, adapted from
#' https://stackoverflow.com/questions/38874741/transform-color-scale-to-probability-transformed-color-distribution-with-scale-f
#'
#' @param x vector to be transformed
#'
#' @importFrom scales label_number
#' @return a \code{scales::trans_new} function
#'
#' @export
quantile_trans <- function(x){
  data_vector <- sort(x)
  scales::trans_new(name="quant_trans",
                    # can I use quantile function here?
                    transform = function(y) findInterval(y,data_vector)/length(data_vector),
                    inverse = function(q) data_vector[1+floor(q*(length(data_vector)-1))],
                    breaks = function(lim,n=5) {
                      # not actually using limits in breaks
                      results <- data_vector[floor(seq(0,1,length.out=n)*(length(data_vector)-1))+1]
                      return(results)
                    },
                    format=scales::label_number()
  )
}


## Prep plotting functions
#############################################################





PrepBar <- function() function(data
    , series_variable = NULL
    , time_variable = NULL
    , time_unit = NULL
    , handle_missing_values  = HandleMissingValues(na_remove = FALSE, na_replace = NULL)
    , handle_zero_values = HandleZeroValues(zero_remove = FALSE, zero_replace = NULL)
  ) {
  data = resolve_var_args(data)
  data = resolve_specific_arg(data, "time_unit", "week")
  output_data = (data
    |> handle_missing_values()
    |> handle_zero_values()
    |> mutate_time_vars(unit = time_unit)
  )
  aggregated = (output_data
    |> group_by_at(vars(get_unit_labels(time_unit)))
    |> summarize(!!series_variable := sum(get(series_variable), na.rm = TRUE))
  )

  output_data = return_iidda(output_data, data)
  attr(output_data, "iidda")$aggregated = aggregated
  return(output_data)
}


#' Prep Data for Bar Graph
#'
#' Prep data for plotting bar graphs. Prep steps were taken from `LBoM::monthly_bar_graph` and `LBoM::weekly_bar_graph`
#' and they include handling missing values and aggregating series data by time unit grouping variable.
#'
#' @param data data frame containing time series data
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param time_variable column name of time variable in `data`, default is "period_end_date"
#' @param time_unit time unit to sum series data over, must be one of iidda.analysis:::time_units, defaults to "week".
#' @param handle_missing_values function to handle missing values, defaults to HandleMissingValues
#' @param handle_zero_values function to handle zero values, defaults to HandleZeroValues
#'
#'
#' @return `data` with records prepped for plotting bar graphs with `series_variable` and `time_unit` field. The name
#' of the resulting `time_unit` field will be named from lubridate_funcs.
#'
#' @concept prep_data_for_plotting
#' @export
iidda_prep_bar <- PrepBar()



#' Prep Data for Box plot
#'
#' Prep data for plotting box plots. Prep steps were taken from `LBoM::monthly_box_plot`
#' and they include handling missing values and creating additional time unit fields.
#'
#' @param data data frame containing time series data
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param time_variable column name of time variable in `data`, default is "period_end_date"
#' @param time_unit time unit to create field from `time_variable`. Must be one of iidda.analysis:::time_units, defaults to "week".
#' @param handle_missing_values function to handle missing values, defaults to HandleMissingValues
#' @param handle_zero_values function to handle zero values, defaults to HandleZeroValues
#'
#'
#' @return all fields in`data` with records prepped for plotting box plots. The name
#' of the new `time_unit` field will be named from lubridate_funcs.
#'
#' @concept prep_data_for_plotting
#' @export
iidda_prep_box <- function(data
    , series_variable = NULL
    , time_variable = NULL
    , time_unit = "month_factor_abbr" #has to be one of iidda.analysis:::time_units
    , handle_missing_values  = HandleMissingValues(na_remove = FALSE, na_replace = NULL)
    , handle_zero_values = HandleZeroValues(zero_remove = FALSE, zero_replace = NULL)
  ) {

  missing_value_handled_data <- handle_missing_values(data,series_variable=series_variable)
  zero_handled_data <- handle_zero_values(missing_value_handled_data,series_variable=series_variable)

  # create time_unit variable
  box_data <- (mutate_time_vars(missing_value_handled_data,unit=time_unit)
               |> mutate(get_unit_labels(time_unit)) ##make it a factor variable
  )

  return(box_data)
}

#' Prep Data for seasonal heatmap
#'
#' Prep data for seasonal heatmap plots. Prep steps were taken from `LBoM::seasonal_heat_map`
#' and they include creating additional time unit fields, splitting weeks that cover the
#' year end,  and optionally normalizing series data to be in the range (0,1).
#'
#' @param data data frame containing time series data
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param start_time_variable column name of time variable in `data`, default is "period_start_date"
#' @param end_time_variable column name of time variable in `data`, default is "period_end_date"
#' @param time_unit a vector of new time unit fields to create from `start_time_variable` and `end_time_variable`.
#' Defaults to "c("yday","year")". The currently functionality expects that both "yday" and "year" are included, should be
#' made more general to incorporate any of iidda.analysis:::time_units.
#' @param prepend_string string to prepend to newly created time_unit fields to distinguish between time_unit
#' fields corresponding to starting versus ending time periods. Defaults to "End ". For example, a `time_unit` of "year"
#' will create a field name "Year" from `start_time_variable` and a field called "End Year" created from `end_time_variable`.
#' @param normalize boolean flag to normalize `series_variable` data to be between 0 and 1.
#' @param ... optional arguments to `year_end_fix()`
#'
#' @importFrom purrr map reduce map2
#' @importFrom scales rescale
#' @return all fields in`data` with records prepped for plotting seasonal heatmaps. The name
#' of the new `time_unit` fields will be named from lubridate_funcs.
#'
#' @concept prep_data_for_plotting
#' @export
iidda_prep_seasonal_heatmap <- function(data
    , series_variable = NULL
    , start_time_variable = "period_start_date"
    , end_time_variable = "period_end_date"
    , time_unit = c("yday","year") #has to be one of iidda.analysis:::time_units
    , prepend_string = "End "
    , normalize = FALSE
    , ...
  ){


  # add starting time unit variables
  add_starting_time <- (map(time_unit, ~ mutate_time_vars(data,input_nm=start_time_variable,unit=.x))
                        |> reduce(left_join, by=colnames(data))
  )

  # add ending time unit variables
  # prepend "End" to each of time_unit unit_labels to prevent duplicate names
  add_ending_time <- (map2(time_unit ,paste0(prepend_string, get_unit_labels(time_unit)), ~ mutate_time_vars(data,input_nm=end_time_variable,unit=.x,output_nm=.y))
                      |> reduce(left_join, by=colnames(data))
  )

  # combine data containing all new time column names
  new_data <- (add_starting_time
                |> left_join(add_ending_time, by=colnames(data))
  )

  # assumes one of time_unit=="year", this should be generalized somehow
  start_year_name <- grep("^(?=.*Year)(?:(?!Day).)*$",colnames(add_starting_time),perl=TRUE,value=TRUE)
  end_year_name <- grep("^(?=.*Year)(?:(?!Day).)*$",colnames(add_ending_time),perl=TRUE,value=TRUE)

  # assumes one of time_unit=="yday", this should be generalized somehow
  start_day_name <- grep("^(?=.*Day).*$",colnames(add_starting_time),perl=TRUE,value=TRUE)
  end_day_name <- grep("^(?=.*Day).*$",colnames(add_ending_time),perl=TRUE,value=TRUE)

  # split weeks that cover the year end
  end_of_year_data <- year_end_fix(new_data,
                                   start_year_variable = start_year_name,
                                   end_year_variable = end_year_name,
                                   start_day_variable = start_day_name,
                                   end_day_variable = end_day_name,
                                   ...)

  # combine year end data with remaning data and optionally normalize
  heat_data <- (new_data
                   |> filter(get(start_year_name)==get(end_year_name))
                   |> union(end_of_year_data)
                   |> group_by(across(start_year_name))
                   %>% {if (normalize) mutate(.,!!series_variable := scales::rescale(get(series_variable),to=c(0,1))) else .}
                   |> ungroup()
  )

  return(heat_data)
}



iidda_prep_heatmap_decomp = function(data
  , grouping_variable
  , series_variable
  , time_scale_picker = TimeScalePicker()
) {
  heatmap_data = time_scale_picker(data)
  ordering = (heatmap_data
    |> group_by(.data[[grouping_variable]])
    |> summarize(.total = mean(.data[[series_variable]]))
    |> ungroup()
    |> arrange(.total)
    |> pull(!!grouping_variable)
  )
  heatmap_data[[grouping_variable]] = factor(
      heatmap_data[[grouping_variable]]
    , levels = ordering
  )
  heatmap_data
}


#' @noRd
iidda_prep_line_agg = function(data
  , period_aggregator = PeriodAggregator()
) {
  period_aggregator(data)
}


#' Prep Data for Rohani Plot
#'
#' Prep data for rohani plots. Prep steps include creating additional time unit fields, summarizing the series
#' variable by time unit and grouping variable (the x and y axis variables) ,and optionally normalizing series
#' data to be in the range (0,1). By default, the grouping variable is ranked in order of the summarized series
#' variable. Needs to be generalized more, might
#' need to handle the case where the desired y-axis is a second time unit, as in the seasonal heatmap plot and therefore
#' making use of the year_end_fix function.
#'
#' @param data data frame containing time series data
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param time_variable column name of time variable in `data`, default is "period_end_date"
#' @param start_time_variable column name of time variable in `data`, default is "period_end_date"
#' @param grouping_variable column name of grouping variable to appear on the y-axis of the heatmap.
#' @param ranking_variable column name of variable used to rank the grouping variable.
#' @param time_unit a vector of new time unit fields to create from `start_time_variable` and `end_time_variable`.
#' Defaults to "c("year")". The currently functionality expects that "year" is included, should be
#' made more general to incorporate any of iidda.analysis:::time_units.
#' @param normalize boolean flag to normalize `series_variable` data to be between 0 and 1.
#' @param handle_missing_values function to handle missing values, defaults to HandleMissingValues
#' @param handle_zero_values function to handle zero values, defaults to HandleZeroValues
#' @param create_nonexistent boolean flag to create \code{NA} records for non-existent `time_unit` and `grouping_variable`.
#' This creates all combinations of `time_unit` and `grouping_variable` to ensure there are no missing records.
#'
#' @importFrom dplyr desc matches
#' @return all fields in`data` with records prepped for plotting rohani heatmaps. The name
#' of the new `time_unit` fields will be named from lubridate_funcs.
#'
#' @concept prep_data_for_plotting
#' @export
iidda_prep_rohani <- function(data
      , series_variable = NULL
      , time_variable = "period_end_date"
      , start_time_variable = "period_start_date"
      #, end_time_variable = "period_end_date" # might not need this we are plotting a second time unit on the y-axis
      , time_unit = "year" #has to be one of iidda.analysis:::time_units
      , grouping_variable = "cause"
      , ranking_variable = NULL #optionally specify the ranking variable to order by?
      #, prepend_string = "End " # might not need this
      , normalize = FALSE
      , handle_missing_values  = HandleMissingValues(na_remove = FALSE, na_replace = NULL)
      , handle_zero_values = HandleZeroValues(zero_remove = FALSE, zero_replace = NULL)
      , create_nonexistent=FALSE
  ){

  # Steps so far
  # 0. optionally handle missing or zero data
  # 1. Create time unit columns
  # 2. Don't need year end fix, because y-axis is cause and not time dependent - might need to generalize later
  # 3. Create ordered vector of grouping_variable -
  # 4. summarize series variable by x and y axis variables (x=time_unit, y=cause), is cause the grouping variable
  # 5. at this point, be nice to optionally normalize the summarized variable


  missing_value_handled_data <- handle_missing_values(data,series_variable=series_variable)
  zero_handled_data <- handle_zero_values(missing_value_handled_data,series_variable=series_variable)



  # 1. add starting time unit variables
  add_time_units <- (map(time_unit, ~ mutate_time_vars(zero_handled_data,input_nm=start_time_variable,unit=.x))
                        |> reduce(left_join, by=colnames(zero_handled_data))
  )

  # assumes one of time_unit=="year", this should be generalized somehow
  start_year_name <- grep("^(?=.*Year)(?:(?!Day).)*$",colnames(add_time_units),perl=TRUE,value=TRUE)


  if(create_nonexistent){

    # get unique grouping variable and time unit variable
    grouping_time_combinations <- expand.grid(unique(add_time_units[[grouping_variable]]),
                                              unique(add_time_units[[start_year_name]])) |> setNames(c(grouping_variable,start_year_name))

    # create all records
    add_time_units <- (grouping_time_combinations
                       |> left_join(add_time_units))
  }


  # how should data be ranked, by default rank data by summarized series variable for each group
  ranking_col <- (add_time_units
                  |> group_by_at(vars(grouping_variable))
                  #to get the ranking group
                  |> summarize(summarized_series_variable = if_else(all(is.na(get(series_variable))), NA_real_, sum(get(series_variable), na.rm = TRUE)))
                  |> arrange(desc(summarized_series_variable))
                  |> select(matches(grouping_variable))
                  |> pull()
  )



  rohani_data <- (add_time_units
                  |> group_by_at(vars(start_year_name,grouping_variable))
                # |> summarize(grouped_deaths = sum(deaths))
                |> dplyr::summarize(!!series_variable := if_else(all(is.na(get(series_variable))), NA_real_, sum(get(series_variable), na.rm = TRUE)))
                # if grouping variable is categorical?
                %>% {if (normalize) mutate(.,!!series_variable := scales::rescale(get(series_variable),to=c(0,1))) else .}
                |> mutate(!!grouping_variable := factor(get(grouping_variable), levels=ranking_col))
                |> arrange(desc(get(grouping_variable))) # not sure if desc is required
  )

 return(rohani_data)
}



#' Prep Data for Periodogram
#'
#' Prep data for plotting peridograms. Prep steps were taken from `LBoM::periodogram`
#' and they include handling missing values,...
#'
#' @param data data frame containing time series data
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param time_variable column name of time variable in `data`, default is "period_end_date"
#' @param time_unit time unit to create field from `time_variable`. Must be one of iidda.analysis:::time_units, defaults to "week".
#' @param normalize boolean flag to normalize `series_variable` data to be between 0 and 1.
#' @param periods_per_year number of time periods per year, default is 52 for weekly data.
#' @param max_period maximum period to appear in plot, default is 10 years.
#' @param handle_missing_values function to handle missing values, defaults to HandleMissingValues
#'
#'
#' @importFrom stats spec.pgram na.fail
#' @return all fields in`data` with records prepped for plotting box plots. The name
#' of the new `time_unit` field will be named from lubridate_funcs.
#'
#' @concept prep_data_for_plotting
#' @noRd
iidda_prep_periodogram <- function(data
    , series_variable = NULL
    , time_variable = NULL
    , transform = FALSE
    , transformation = "log10"
    , spans = NULL
    , kernel = NULL
    , taper = 0.1
    , pad = 0
    , fast = TRUE
    , demean = FALSE
    , detrend = TRUE
    , na.action = na.fail
    # normalize spectrum to [0,1]
    , normalize = TRUE
    # time periods in a year (52 weeks in a year), do we need to account for other time units
    , periods_per_year = 52
    , max_period = 10
    , handle_missing_values = HandleMissingValues(na_remove = TRUE, na_replace = NULL)
  ) {

  harmonized_data <- handle_missing_values(data,series_variable=series_variable)

  # tranforming series variable before passing to periodogram...

  if(transform){
    harmonized_data <- mutate(across(harmonized_data,series_variable,transformation))
  }

  ## compute periodogram
  x <- spec.pgram(harmonized_data[[series_variable]],
                  plot=FALSE,
                  spans = spans,
                  kernel=kernel,
                  taper = taper,
                  pad = pad,
                  fast = fast,
                  demean = demean,
                  detrend = detrend,
                  na.action = na.action)

  ## convert period to years
  x$per <- 1/(periods_per_year*x$freq)
  if (normalize) x$spec <- x$spec / max(x$spec)

  # final data prep for plotting
  prepped_data <- (cbind(per=x$per,spec=x$spec)
                   |> data.frame()
                   |> filter(per <= max_period)
  )

  return(prepped_data)

}

#' Prep Data for Wavelet Plot
#'
#' Prep data for wavelet plot. Prep steps were taken from code provided by Steven Lee
#' (https://github.com/davidearn/StevenLee) and Kevin Zhao (https://github.com/davidearn/KevinZhao).
#'
#' @param data data frame containing time series data
#' @param trend_data data frame containing time series trend data
#' @param time_variable column name of time variable in `data`, default is "period_end_date"
#' @param series_variable  column name of series variable in `data`, default is "deaths_series"
#' @param trend_variable column name of series variable in `data`, default is "deaths_trend"
#' @param series_suffix suffix to be appended to series data fields
#' @param trend_suffix suffix to be appended to trend data fields
#' @param wavelet_variable name of the field in `data` to be wavelet transformed
#' @param output_emd_trend name of output field for the empirical mode decomposition applied to `trend_variable`
#' @param output_norm name of output field for the `series_variable` normalized by `output_emd_trend`
#' @param output_sqrt_norm name of output field for the square root of `output_norm`
#' @param output_log_norm name of output field for the logarithm of (`output_norm` + `eps`)
#' @param output_emd_norm name of output field for the empirical mode decomposition applied to `output_norm`
#' @param output_emd_sqrt name of output field for the empirical mode decomposition applied to `output_sqrt_norm`
#' @param output_emd_log name of output field for the empirical mode decomposition applied to `output_log_norm`
#' @param output_detrend_norm name of output field for the computed field `output_norm`-`output_emd_norm`
#' @param output_detrend_sqrt name of output field for the computed field `output_sqrt_norm`-`output_emd_sqrt`
#' @param output_detrend_log name of output field for the computed field `output_log_norm`-`output_emd_log`
#' @param data_harmonizer function that harmonizes time scales and series names so there is one
#' data point per time unit
#' @param trend_data_harmonizer function that harmonizes time scales and trend names so there is one
#' data point per time unit
#' @param data_deheaper function that fixes heaping errors on series data
#' @param trend_deheaper function that fixes heaping errors on trend data
#' @param joiner function that joins series and trend data sets
#' @param interpolator function that linearly interpolates series and trend data
#' @param normalizer function that computes normalized fields
#' @param transformer function that computes wavelet transform
#'
#'
#' @importFrom lubridate dhours
#' @return list containing:
#'        * \code{transforemd_data} - wavelet transformed data
#'        * \code{tile_data_to_plot} - data set of the wavelet transformed data
#' prepped for plotting with \code{ggplot2::geom_tile}
#'        * \code{contour_data_to_plot} - data set of the transformed wavelet data prepped
#' for plotting with \code{ggplot2::geom_contour}
#'
#' @concept prep_data_for_plotting
#' @name wavelet_something
NULL




GetMetadata = function() function(data
    , time_variable = NULL
    , descriptor_variable = NULL
    #, time_unit, might want this
  ) {
  data = resolve_var_args(data)
  min_time <- min(data[[time_variable]],na.rm=TRUE)
  max_time <- max(data[[time_variable]],na.rm=TRUE)
  descriptor_name <- data |> select(all_of(descriptor_variable)) |> unique()

  metadata = list(
      min_time = min_time
    , max_time = max_time
    , descriptor_name = descriptor_name
  )
  return(metadata)
}

#' Get IIDDA metadata
#'
#' Get starting time period, ending time period and mortality cause name from the
#' data set for use in axis and main plot titles.
#'
#' @param data data frame containing time series data
#' @param time_variable column name of time variable in `data`, default is "period_end_date"
#' @param descriptor_variable column name of the descriptor variable in `data`, default is "cause" for
#' mortality data sets.
#'
#' @return a list in order containing minimum time period, maximum time period and cause name.
#' @export
iidda_get_metadata = GetMetadata()



## Functions that take a data set and return a plot (or partial plot)
#####################################################################


#' Plot Moving Average Time Series
#'
#' Add a moving average time series line to an exiting ggplot plot object. Graphical choices were made
#' to closely reflect plots generated with `LBoM::plot.LBoM`.
#'
#' @param plot_object a `ggplot2` plot object
#' @param data data frame containing moving average time series data, typically output from `iidda_prep_ma()`.
#' If `NULL` data is inherited from `plot_object`
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param time_variable column name of time variable in `data`, default is "period_end_date"
#'
#' @importFrom ggplot2 geom_line scale_y_continuous
#' @return a ggplot2 plot object containing a moving average time series
#' @concept plotting_functions
#' @export
iidda_plot_ma <- function(plot_object, data = NULL
     , series_variable = NULL
     , time_variable = NULL
  ){

  (plot_object
   + geom_line(data=data, aes(x=.data[[time_variable]],y=.data[[series_variable]]))
   # + labs(x=time_variable,y=series_variable)
   + scale_y_continuous()
  )
}


iidda_plot_line_agg = function(plot_object
  , data = NULL
  , series_variable = NULL ## "daily_rate"
  , time_variable = NULL ## "period_mid_time"
  , contiguous_variable = NULL
  , trans = scales::sqrt_trans()
) {
  spec = aes(
      x = .data[[time_variable]]
    , y = .data[[series_variable]]
  )
  # if (!is.null(contiguous_variable)) {
  #   spec = aes(
  #       x = .data[[time_variable]]
  #     , y = .data[[series_variable]]
  #     #, group = .data[[contiguous_variable]]
  #   )
  # }
  (plot_object
    + geom_line(spec, data)
    + scale_y_continuous(trans = trans)
    + scale_x_datetime(expand = c(0, 0))
    + iidda_theme_above()
  )
}


#' IIDDA Heatmap Decomposition
#'
#' @param data Named list of two data frames: `heatmap` for the heatmap
#' component and `line` for the line-graph component. This list can be
#' computed using `iidda_prep_heatmap_decomp`
#' @importFrom patchwork plot_layout
#' @importFrom ggplot2 dup_axis
#' @noRd
iidda_plot_heatmap_decomp = function(plot_object
  , data = NULL
  , grouping_variable
  , series_variable = NULL
  , time_variable = NULL ## "period_mid_time"
  , num_days_variable = "num_days"  ## TODO: should be able to get this from period_aggregator
  , contiguous_variable = NULL
  , trans = scales::sqrt_trans()
  , n_colours = scales::brewer_pal(palette = "YlOrRd")(9)
  , NA_colour = "black"
  , period_aggregator = PeriodAggregator()
  , layout_proportions = c(1, 3)
  , rate_title = "" ## default is no title
  , grouping_title = "" ## default is no title
) {
  spec = aes(
      x = .data[[time_variable]]
    , y = .data[[grouping_variable]]
    , width = 86400 * .data[[num_days_variable]]
    , fill = .data[[series_variable]]
  )
  ## TODO: units and titles should be an argument or automatically determined
  #rate_title = expression(atop('Incidence rate', '(' ~ days^-1 ~ 10^-5 ~ ')'))
  #rate_heat_title = ""
  #prov_title = "Province / Territory"
  heatmap = (plot_object
    + geom_tile(spec, data)
    + scale_fill_gradientn(
      colours = n_colours,
      trans = trans,
      na.value = NA_colour
    )
    + scale_x_datetime(expand = c(0,0), sec.axis = dup_axis())
    + labs(fill = "")
    + ylab(grouping_title)
    + iidda_theme_heat()
  )
  if (is.null(data)) data = plot_object$data
  lineplot = (data
    |> iidda_prep_line_agg(period_aggregator)
    |> ggplot()
    |> iidda_plot_line_agg(
        time_variable = time_variable
      , series_variable = series_variable
      , contiguous_variable = contiguous_variable
    )
    + ylab(rate_title)
  )

  ## TODO: this should happen outside of the package in a user script.
  ##       patchwork is just too easy and useful to bury like this.
  lineplot / heatmap + plot_layout(heights = layout_proportions)
}






#' @export
iidda_ma = function(data
    , series_variable = NULL
    , time_variable = NULL
    , trim_series = TrimSeries(zero_lead = FALSE, zero_trail = FALSE)
    , handle_missing_values = HandleMissingValues(na_remove = FALSE, na_replace = NULL)
    , handle_zero_values = HandleZeroValues(zero_remove = FALSE, zero_replace = NULL)
    , compute_moving_average = ComputeMovingAverage(ma_window_length = 52)
    , time_variable_converter = TimeVariableConverter()
) {
  metadata = iidda_get_metadata(data, time_variable = time_variable)
  ma_data = iidda_prep_ma(data
    , series_variable
    , time_variable
    , trim_series
    , handle_missing_values
    , handle_zero_values
    , compute_moving_average
    , time_variable_converter
  )
  (ggplot()
    |> iidda_plot_series(ma_data, series_variable, time_variable)
    |> iidda_plot_settings(metadata)
  )
}


AttachBar = function() function(data
    , initial_ggplot_object = ggplot()
    , series_variable = NULL
    , time_unit = NULL
    , aggregated = NULL
  ) {
  data = resolve_var_args(data)
  data = resolve_specific_arg(data, "aggregated", NULL)
  data = resolve_specific_arg(data, "time_unit", NULL)
  plot = (initial_ggplot_object
    + aes(x = .data[[get_unit_labels(time_unit)]], y = .data[[series_variable]])
    + geom_col(data = aggregated)
    + scale_y_continuous(name = make_axis_title(series_variable), expand = c(0, 0))
  )
  iidda_defaults_if(data, plot = plot)
}

#' Attach Bar Plot to Dataset
#' 
#' @param data Data frame, probably containing an IIDDA dataset.
#' @param initial_ggplot_object Plot object that will be used to add a
#' bar geom.
#' 
#' @export
iidda_attach_bar = AttachBar()


#' Plot Bar Graph
#'
#' Add a bar plot to an exiting ggplot plot object. Graphical choices were made to closely reflect
#' plots generated with `LBoM::monthly_bar_graph` and `LBoM::weekly_bar_graph`.
#'
#' @param plot_object a `ggplot2` plot object
#' @param data data frame containing data prepped for bar plotting, typically output from `iidda_prep_bar()`.
#' If `NULL` data is inherited from `plot_object`
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param time_unit time unit to display bar graphs on the x-axis. Defaults to "week" or one of iidda.analysis:::time_units that starts
#' with "month". Should generalize at some point to be able to take any time_unit argument.
#'
#' @importFrom grDevices rainbow
#' @importFrom ggplot2 geom_col aes scale_y_continuous
#' @return a ggplot2 plot object containing a bar graphs of time series data
#'
#' @concept plotting_functions
#' @export
iidda_plot_bar <- function(plot_object
    , data = NULL
    , series_variable = NULL
    , time_unit = "month_factor_abbr"
  ) {
  (plot_object
    + aes(x = .data[[get_unit_labels(time_unit)]], y = .data[[series_variable]])
    + geom_col(data = data)
    + scale_y_continuous(name = make_axis_title(series_variable), expand = c(0, 0))
  )
}





#' Get IIDDA Attribute
#' 
#' @param data Data frame that contains a list attribute called `"iidda"`.
#' @param which Name of the element in the `"iidda"` list to extract.
#' @returns Value of the element given by `which` in the `"iidda"` attribute.
#' @export
get_iidda_attr = function(data, which) {
  (data
    |> attr("iidda")
    |> getElement(which)
  )
}


resolve_arg_guesses = function(data) {
  nms = help_msg_about_guesses(data)
  for (nm in nms) attr(attr(data, "iidda")[[nm]], "guess") = NULL
  return(data)
}
help_msg_about_guesses = function(data) {
  attr_nms = data |> attr("iidda") |> names()
  for (attr in attr_nms) {
    if (is_guess(data, attr) & FALSE) {
      message(
          "We guessed about the names of certain variables. ",
          "It is safer to use ?iidda_defaults() to explicitly declare them."
      )
      return(attr_nms)
    }
  }
  return(attr_nms)
}



#' @importFrom ggplot2 ggplot
#' @export
iidda_bar = function(data
    , series_variable = NULL
    , time_variable = NULL
    , time_unit = "month_factor_abbr" # has to be one of iidda.analysis:::time_units
    , handle_missing_values  = HandleMissingValues(na_remove = FALSE, na_replace = NULL)
    , handle_zero_values = HandleZeroValues(zero_remove = FALSE, zero_replace = NULL)
    , title = TitleGuesser()
    , subtitle = TimeRangeDescriber()
    , theme = iidda_theme
  ) {
  (data
    |> iidda_prep_bar(
        series_variable
      , time_variable
      , time_unit
      , handle_missing_values
      , handle_zero_values
    )
    |> iidda_attach_bar(ggplot(), series_variable, time_unit)
    |> iidda_render_plot(title, subtitle, theme)
  )
}

#' Plot Box Plot
#'
#' Add a box plot to an exiting ggplot plot object.  Graphical choices were made to closely reflect
#' plots generated with `LBoM::monthly_box_plot`.
#'
#' @param plot_object a `ggplot2` plot object
#' @param data data frame containing data prepped for box plotting, typically output from `iidda_prep_box()`.
#' If `NULL` data is inherited from `plot_object`
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param time_unit time unit to display box plots on the x-axis. Defaults to "week", should be able to
#' handle any time_unit from iidda.analysis:::time_units.
#' @param ... other arguments to be passed to `scale_x_discrete`
#'
#' @importFrom ggplot2 geom_boxplot scale_x_discrete
#' @return a ggplot2 plot object containing a box plots of time series data
#'
#' @concept plotting_functions
#' @export
iidda_plot_box <- function(plot_object,
                          data=NULL,
                          series_variable="deaths",
                          time_unit="week",
                          ...){


  plot_object + geom_boxplot(data=data, aes(x= as.factor(.data[[get_unit_labels(time_unit)]]), y= .data[[series_variable]])) + scale_x_discrete(...)

}

#' Plot Heatmap
#'
#' Add a yearly vs. weekly heatmap to an exiting ggplot plot object. Graphical choices were made to closely reflect
#' plots generated with`LBoM::seasonal_heat_map`.
#'
#' @param plot_object a `ggplot2` plot object
#' @param data data frame containing data prepped for yearly vs. weekly heatmaps, typically output from
#' `iidda_prep_heatmap()`. If `NULL` data is inherited from `plot_object`.
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param start_year_variable column name of time variable containing the year of the starting period, defaults to "Year"
#' @param end_year_variable column name of time variable containing the year of the ending period, defaults to "End Year"
#' @param start_day_variable column name of time variable containing the day of the starting period, defaults to "Day of Year"
#' @param end_day_variable column name of time variable containing the day of the ending period, defaults to "End Day of Year"
#' @param colour_trans string indicating colour transformation, one of "log2", "sqrt" or "linear"
#' @param NA_colour colour for `NA` values, defaults to "black"
#' @param palette_colour colour of heatmap palette, defaults to "RdGy". Should specify what type of palette colours
#' are accepted by this argument.
#' @param ... Not currently used.
#'
#' @importFrom ggplot2 ggplot_build geom_rect scale_x_continuous scale_y_continuous scale_fill_distiller xlab ylab
#' @importFrom lubridate yday
#' @return a ggplot2 plot object containing a yearly vs. weekly heatmap of time series data
#'
#' @concept plotting_functions
#' @export
iidda_plot_heatmap <- function(plot_object,
                              data=NULL,
                              series_variable="deaths",
                              start_year_variable = "Year",
                              end_year_variable = "End Year",
                              start_day_variable = "Day of Year",
                              end_day_variable = "End Day of Year",
                              colour_trans ="log2",# one of log2, sqrt, linear?
                              NA_colour = "black",
                              palette_colour = "RdGy",
                              ...){
  # extract data set from plot_object
  get_data <- ggplot_build(plot_object)
  # get max value of series variable
  max_val <- max(get_data$plot$data[,series_variable], na.rm = TRUE)

  # can this be done with ggplot...
  # taken from LBoM::seasonal_heat_map
  if (colour_trans=="log2") {
    max_break <- round(log(base = 2, max_val))
    colour_breaks <- c(2^(0:max_break + 1))
  }
  if (colour_trans=="sqrt") {
    max_break <- round(sqrt(max_val))
    colour_breaks <- c((0:max_break+1)^2)
  }
  if (colour_trans=="linear") {
    break_div <- max_val %/% 10 + 1
    colour_breaks <- c(break_div * (0:10))
  }


  # get day of the year number for the first of every month for axis breaks
  monthStarts <- yday(as.Date(paste0(month.abb, "-01"),format="%b-%d"))

  (plot_object +
      # it would be nice to implement with geom_raster() but the tiles are not all the same size
      geom_rect(mapping=aes(xmin = get(start_year_variable),
                                      xmax = (get(start_year_variable)+1),# why not use end_year?
                                      ymin = get(start_day_variable),
                                      ymax = get(end_day_variable),
                                      fill = get(series_variable))) +
      scale_y_continuous(breaks = monthStarts, labels = month.abb, expand=c(0,0)) +
      scale_x_continuous(expand=c(0,0)) +
      scale_fill_distiller(values = scales::rescale(colour_breaks, c(0,1)),
                                                    palette = palette_colour, name = series_variable,
                                                    na.value = NA_colour) +
      xlab(start_year_variable) +
      ylab(start_day_variable) # should this say week?
  )

}




#' Plot Rohani Heatmap
#'
#' Add a rohani heatmap to an exiting ggplot plot object. Possibly to be extended to include time series in
#' a separate facet.
#'
#' @param plot_object a `ggplot2` plot object
#' @param data data frame containing data prepped for yearly vs. weekly heatmaps, typically output from
#' `iidda_prep_heatmap()`. If `NULL` data is inherited from `plot_object`.
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param start_year_variable column name of time variable containing the year of the starting period, defaults to "Year"
#' @param end_year_variable column name of time variable containing the year of the ending period, defaults to "End Year"
#' @param start_day_variable column name of time variable containing the day of the starting period, defaults to "Day of Year"
#' @param end_day_variable column name of time variable containing the day of the ending period, defaults to "End Day of Year"
#' @param grouping_variable column name of grouping variable to appear on the y-axis of the heatmap.
#' @param colour_trans function to scale colours, to be supplied to trans argument of scale_fill_gradientn()
#' @param n_colours vector of colours to be supplied to scale_fill_gradientn()
#' @param NA_colour colour for `NA` values, defaults to "black"
#' @param palette_colour colour of heatmap palette, defaults to "RdGy". Should specify what type of palette colours
#' are accepted by this argument.
#'
#' @importFrom ggplot2 geom_raster scale_y_discrete scale_fill_gradientn labs theme_bw theme element_blank
#' @return a ggplot2 plot object containing a yearly vs. weekly heatmap of time series data
#'
#' @concept plotting_functions
#' @export
iidda_plot_rohani_heatmap <- function(plot_object,
                              data=NULL,
                              series_variable="deaths",
                              start_year_variable = "Year",
                              end_year_variable = "End Year",
                              start_day_variable = "Day of Year",
                              end_day_variable = "End Day of Year",
                              grouping_variable = "cause",
                              colour_trans = log1p_modified_trans(),
                              #colour_trans = my_trans(data[series_variable]),
                              #colour_trans = pseudo_log_trans(),
                              #colour_breaks = log_breaks(n=5,base="log1p"),
                              n_colours = scales::brewer_pal(palette="YlOrRd")(9),
                              NA_colour = "black",
                              palette_colour = "YlOrRd"){

  # extract data set from plot_object
  get_data <- ggplot_build(plot_object)
  # get max value of series variable
  max_val <- max(get_data$plot$data[,series_variable], na.rm = TRUE)


  (plot_object +
      geom_raster(mapping=aes(x = get(start_year_variable),
                              y = get(grouping_variable),
                              fill = get(series_variable))) +
      scale_y_discrete(limits=rev) + # what is the default sorting, maybe reverse factor levels so I don't need this
      scale_fill_gradientn(#colours=rainbow(250, start=0, end=0.7, rev=TRUE),
                         #colours = scales::seq_gradient_pal()(log1p(seq(0, 1, length.out = 25))),
                         #colours = scales::brewer_pal(palette="YlOrRd")(9),
                          colours = n_colours,
                          #trans = colour_trans(pull(get_data$plot$data[,series_variable])),na.value = "black")+
                          trans = colour_trans,na.value = NA_colour#, breaks = colour_breaks#, labels=log_breaks(6)#, breaks=c(0,5,10,100,1000),labels=c(0,5,10,100,1000)
                          ) +
      scale_x_continuous(expand=c(0,0)) +
      xlab(start_year_variable) +
      ylab(grouping_variable) +
      labs(fill=series_variable) +
      theme_bw() +
      theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
  )


}

#' Plot Periodogram
#'
#' Add a rectangular highlighted region to an existing ggplot2 plot object
#'
#' @param plot_object a `ggplot2` plot object
#' @param data data frame containing time series data. If `NULL` data is inherited from `plot_object`.
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param time_variable column name of time variable in `data`, default is "period_start_date"
#' @param filter_variable column name of variable to filter on in `data`, default is "period_start_date"
#' @param filter_start value of `filter_variable` for starting range, default is "1700-01-01"
#' @param filter_end value of `filter_variable` for ending range, default is "1800-01-01"
#' @param ... other arguments to be passed to `ggforce::geom_mark_rect`, for example annotating with text
#'
#' @importFrom ggplot2 ggtitle
#' @return a ggplot2 plot object a rectangular plot highlight
#'
#' @concept plotting_functions
#' @noRd
iidda_plot_periodogram <- function(plot_object,
                                  data=NULL,
                                  period_variable="per",
                                  spectral_density_variable="spec",
                                  x_variable_name = "Period",
                                  y_variable_name = "Spectral Density",
                                  max_period=10){

  (plot_object
     + geom_line(mapping = aes(x=get(period_variable),y=get(spectral_density_variable)))
     + scale_x_continuous(breaks = seq(0,max_period,2))
     + ggtitle(paste0(start," to ", end))
     + labs(x=x_variable_name,y=y_variable_name)
  )

}


#' Plot Wavelet
#'
#' Plot wavelet to look similar to base R plot of \code{WaveletComp::wt.image} using ggplot2 functionality.
#' Some visual choices were made to reflect work done by Steven Lee (https://github.com/davidearn/StevenLee)
#' and Kevin Zhao (https://github.com/davidearn/KevinZhao).
#'
#' @param plot_object a `ggplot2` plot object
#' @param data data frame containing wavelet data prepped for use in \code{ggplot2::geom_tile}.
#' The output from \code{iidda_prep_wavelet} produces a data set prepped for this argument, named \code{tile_data_to_plot} in the returned list.
#' If `NULL` data is inherited from `plot_object`.
#' @param wavelet_data list containing raw wavelet transformed data, typically output from \code{WaveletComp::analyze.wavelet}.
#' The output from \code{iidda_prep_wavelet} produces a data set prepped for this argument, named \code{transformed_data} in the returned list.
#' @param contour_data data set containing contour data prepped for use in \code{ggplot2::geom_contour}.
#' The output from \code{iidda_prep_wavelet} produces a data set prepped for this argument, named \code{cont_data_to_plot} in the returned list.
#' @param y_variable_name name of y variable in plot, defaults to "Period (years)".
#' @param fill_variable_name name of colour fill variable in plot, defaults to "Power".
#' @param max_period maximum period to appear on the plot, defaults to 10 years.
#' @param colour_levels number of colours to pass to \code{scale_fill_gradientn}.
#' @param start_hue starting hue colour to pass to \code{scale_fill_gradientn}, default taken from WaveletComp::wt.image.
#' @param end_hue ending hue colour to pass to \code{scale_fill_gradientn}, default taken from WaveletComp::wt.image.
#' @param sig_lvl significance level for white contours
#'
#' @importFrom ggplot2 geom_tile scale_x_datetime geom_contour geom_polygon
#' @return a ggplot2 object of a wavelet
#'
#' @concept plotting_functions
#' @export
iidda_plot_wavelet <- function(plot_object,
                                  data=NULL,
                                  wavelet_data,
                                  contour_data,
                                  y_variable_name = "Period (years)",
                                  fill_variable_name = "Power",
                                  max_period=10,
                                  colour_levels = 250,
                                  start_hue = 0,
                                  end_hue = 0.7,
                                  sig_lvl = 0.05){

  ## extract data set from plot_object
  if (is.null(data)) {
    data_to_plot <- ggplot_build(plot_object)$plot$data
  } else {
    data_to_plot = data
  }


  ## Compute edges of cone of influence for geom_polygon
  ymin= min(data_to_plot$y_loc)-0.5*data_to_plot$y_ht[which.min(data_to_plot$y_loc)]
  ymax=max(data_to_plot$y_loc)+0.5*data_to_plot$y_ht[which.max(data_to_plot$y_loc)]
  first_date <-min(data_to_plot$x_loc)-0.5*data_to_plot$x_wid[which.min(data_to_plot$x_loc)]
  end_date <- max(data_to_plot$x_loc)+0.5*data_to_plot$x_wid[which.max(data_to_plot$x_loc)]

  plot_edges  <- data.frame(date=c(end_date,end_date,first_date,first_date),
                            y=c(ymin,ymax,ymax,ymin)
  )

  get_indexes <- (data.frame(date=as_datetime(wavelet_data$series$date),
                             axis_1=wavelet_data$axis.1)
                  |> full_join(data.frame(cbind(coi=wavelet_data$coi.1,
                                                 y=wavelet_data$coi.2)),
                                by=c("axis_1"="coi"),keep=TRUE
                  )
                  |> filter(!is.na(date))
                  |> filter(y >= ymin)
                  |> filter(y <= ymax)
                  |> select(-coi,-axis_1)
                  |> mutate(y=if_else(date==min(date),ymin,y))
                  |> mutate(y=if_else(date==max(date),ymin,y))
                  |> union(plot_edges)

  )


  # Final plot
  (ggplot((data_to_plot))
    + geom_tile(aes(x = x_loc, y = y_loc, width = x_wid, height = y_ht, fill = z))
    # show on Period scale
    + scale_y_continuous(labels=function(y) 2^(y),expand = c(0,0))
    + scale_x_datetime(expand=c(0,0))
    + scale_fill_gradientn(colours=rainbow(colour_levels, start=start_hue, end=end_hue, rev=TRUE),
                           trans = quantile_trans(data_to_plot$z))
    + geom_contour(data=contour_data,aes(x = x_loc, y = y_loc,z=cont),colour="white"
                     ,breaks=c(sig_lvl)
    )
    + geom_contour(data=contour_data, aes(x = x_loc, y = y_loc,z=ridge),colour="black"
                     ,lwd=0.4
    )
    + geom_polygon(data=get_indexes,aes(x=date,y=y),fill="white",alpha=0.5)
    + labs(x="", y=y_variable_name, fill=fill_variable_name)

  )

}




#' Add Plot Highlight
#'
#' Add a rectangular highlighted region to an existing ggplot2 plot object
#'
#' @param plot_object a `ggplot2` plot object
#' @param data data frame containing time series data. If `NULL` data is inherited from `plot_object`. This
#' has only been tested with data output from `iidda_plot_ma`.
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param time_variable column name of time variable in `data`, default is "period_end_date"
#' @param filter_variable column name of variable to filter on in `data`, default is "period_end_date"
#' @param filter_start value of `filter_variable` for starting range, default is "1700-01-01"
#' @param filter_end value of `filter_variable` for ending range, default is "1800-01-01"
#' @param ... other arguments to be passed to `ggforce::geom_mark_rect`, for example annotating with text
#'
#' @importFrom ggforce geom_mark_rect
#' @return a ggplot2 plot object a rectangular plot highlight
#' @concept plotting_functions
#' @export
iidda_plot_highlight <- function(
    plot_object,
    data=NULL,
    series_variable="deaths",
    time_variable="period_end_date",
    filter_variable="period_end_date", # variable to filter on
    filter_start = "1700-01-01", # how to do NULL
    filter_end = "1800-01-01",
    ...){

  plot_object + geom_mark_rect(data, mapping=aes(x=.data[[time_variable]], y=.data[[series_variable]], filter = get(filter_variable) >= filter_start & get(filter_variable) <= filter_end, ...), expand=0,...)


}

#' Scale series data
#'
#' Scale time series data by transformation.
#'
#' @param plot_object a `ggplot2` plot object
#' @param data data frame containing time series data. If `NULL` data is inherited from `plot_object`.
#' @param scale_transform transformation to apply to \code{y} variable, must be a valid ggplot2 transformation.
#'
#' @return a ggplot2 plot object with scaled \code{y} data
#' @noRd
iidda_plot_scales <- function(
    plot_object,
    data=NULL,
    scale_transform = "log1p"
  ){
  ## Probably easier to not use something like this and just
  ## define want you want for default scales for each plot type.
  plot_object + scale_y_continuous(trans = scale_transform)
}


#' LBoM plot settings
#'
#' Add basic features to a ggplot2 plot object including title, subtitle and classic `ggplot2::theme_bw` theme.
#'
#' @param plot_object a `ggplot2` plot object
#' @param data list containing metadata. If `NULL` data is inherited from `plot_object`.
#' @param min_time name of field in data containing the minimum time period range, defaults to "min_time".
#' @param max_time name of field in data containing the minimum time period range, defaults to "max_time".
#' @param descriptor_name either the name of a field in data containing the descriptor or a string to be used as the
#' plot title. If there are too more than 3 elements in the descriptor field, then `descriptor_variable` is used as the plot
#' title.
#' @param theme ggplot theme
#'
#' @return a ggplot2 plot object with title, subtitle and adjusted theme.
#'
#' @concept plotting_functions
#' @export
iidda_plot_settings <- function(plot_object
     , data = data.frame()
     , min_time = "min_time"
     , max_time = "max_time"
     , descriptor_name = "descriptor_name"
     , theme = iidda_theme
  ) {

  # if there are too many descriptors or the descriptor name doesn't exist in data,
  # use the provided descriptor string to label the plot
  if ( !is.null(data[[descriptor_name]])){
    if ( nrow(data[[descriptor_name]]) > 3){
      descriptor_name <- data[[descriptor_name]] |> pull()
    } else {
      descriptor_name <- colnames(data[[descriptor_name]])
    }
  }

  iidda_title(plot_object
    , descriptor_name
    , paste(c(data[[min_time]], data[[max_time]]), collapse = " to ")  # this is rubbish
  )

}



iidda_title = function(plot_object, title, subtitle) {
  UseMethod("iidda_title")
}

#' @export
iidda_title.gg = function(plot_object, title, subtitle) {
  plot_object + ggtitle(label = title, subtitle = subtitle)
}

#' @export
iidda_title.patchwork = function(plot_object, title, subtitle) {
  plot_object + plot_annotation(title = title, subtitle = subtitle)
}




#' Get time unit labels
#'
#' Get label of associated time unit
#'
#' @param unit time unit, one of iidda.analysis:::time_units
#'
#' @return label of associated time unit
#' @export
get_unit_labels = function(unit) {
  lubridate_funcs[sub("^([a-z]+)_[a-z0-9_]+$", "\\1", unit, perl = TRUE)]
}



#' Mutate time variables
#'
#' Create new time unit fields
#'
#' @param data data set containing an input time field
#' @param unit time unit, one of iidda.analysis:::time_units
#' @param input_nm field name in `data` containing input time field
#' @param output_nm field name of newly created time unit field, by default uses get_unit_labels().
#'
#' @return all fields in `data` with additional time unit field
#' @export
mutate_time_vars = function(
  data,
  unit = unname(time_units),
  input_nm = "period_end_date",
  output_nm = get_unit_labels(unit)
) {
  #browser()
  #stopifnot(valid_time_vars(input_nm, data))
  time_unit_func = make_time_trans(unit)
  mutate(data, !!output_nm := time_unit_func(get(input_nm)))
}

#' Get time transformation
#'
#' Get associated lubridate function to compute time unit.
#'
#' @param unit time unit, one or more of iidda.analysis:::time_units
#'
#' @importFrom utils getFromNamespace
#' @importFrom lubridate wday mday qday yday week epiweek isoweek month quarter year
#' @return function to compute time unit
#' @export
make_time_trans = function(unit = unname(time_units)) {

  unit = match.arg(unit)
  as_func = force
  lubridate_func_nm = sub("^([a-z]+)_[a-z0-9_]+$", "\\1", unit, perl=TRUE)
  lubridate_func = getFromNamespace(lubridate_func_nm, "lubridate")
  if (lubridate_func_nm %in% c("month", "wday")) {
    type = sub("^[a-z]+_([a-z0-9]*)[_a-z]*$", "\\1", unit, perl=TRUE)
    abbr = sub("^[a-z]+_[a-z0-9]+_([a-z]*)$", "\\1", unit, perl=TRUE) == "abbr"
    label = type != "num"
    if (type == "factor") as_func = getFromNamespace("factor", "base")
    unit_func = function(x) lubridate_func(x, label = label, abbr = abbr)
  } else if (lubridate_func_nm == "quarter") {
    fiscal_start = as.integer(sub("^[a-z]+_([0-9]+)$", "\\1", unit, perl=TRUE))
    unit_func = function(x) {
      lubridate_func(x, type = "quarter", fiscal_start = fiscal_start)
    }
  } else {
    unit_func = lubridate_func
  }

  function(x) as_func(unit_func(x))
}


make_axis_title = function(variable_name) {
  dict = iidda_data_dictionary()
  if (inherits(dict, "try-error")) return(variable_name)
  return(dict[[variable_name]]$title)
}




#' Basal Group Adder
#'
#' @param data A tidy data set with a `disease` column.
#' @param lookup A lookup table with `disease` and `nesting_disease`
#' columns that describe a global disease hierarchy that will be applied
#' to find the basal disease of each `disease` in data.
#'
#' @return tidy dataset with basal disease
#' @export
BasalGroupAdder = function(lookup) {
  function(data, hierarchical_variable = NULL, nesting_variable = NULL) {

    data = resolve_var_args(data
      , flush_arg_guesses = FALSE
      , ign_variables = "nesting_variable"
    )
    data = resolve_specific_arg(data
      , "nesting_variable"
      , sprintf("nesting_%s", hierarchical_variable)
    )
    check_vars_in_data(data, "nesting_variable", nesting_variable)

    hierarchy = (lookup
      |> select(all_of(c(hierarchical_variable, nesting_variable)))
      |> distinct()
    )
    all_values = data[[hierarchical_variable]] |> unique()
    is_missing = !all_values %in% hierarchy[[hierarchical_variable]]
    if (any(is_missing)) {
      missing_values = (data
        |> select(all_of(c(hierarchical_variable, nesting_variable)))
        |> filter(.data[[hierarchical_variable]] %in% all_values[is_missing])
        |> distinct()
      )
      hierarchy = rbind(missing_values, hierarchy)
    }

    ## TODO: generalize basal_disease
    with_basal = (data
      |> rowwise()
      |> mutate(basal_group = basal_group(.data[[hierarchical_variable]]
        , hierarchy
        , hierarchical_variable
        , nesting_variable
      ))
      |> ungroup()
    )

    return_iidda(with_basal, data)
  }
}

#' Basal Group
#'
#' @param value Value (e.g., `hepatitis-A`) of the `hierarchical_variable`
#' (e.g., `disease`) for which to determine basal value (e.g., `hepatitis`).
#' @param lookup Table with two character-valued columns with names given by
#' `hierarchical_variable` (e.g., `disease`) and `nesting_variable`
#' (e.g., `nesting_disease`).
#' @param hierarchical_variable Name of the hierarchical variable in `lookup`
#' @param nesting_variable Name of the nesting variable in `lookup`
#' @param encountered_values Character vector of values already found.
#' Typically this left at the default value of an empty character vector.
#'
#' @return The root value that input value maps to in `lookup`.
#'
#' @export
basal_group = function(value, lookup, hierarchical_variable, nesting_variable, encountered_values = character()) {
  focal_values = get_focal_values(value, lookup, hierarchical_variable, nesting_variable)
  check_missing_nodes(value, focal_values, nesting_variable)
  is_duplicate_nodes = length(focal_values) > 1L
  if (is_duplicate_nodes) {
    lookup = unique(lookup)
    focal_values = unique(focal_values)
    check_multiple_focal_values(value, focal_values, hierarchical_variable)
  }
  if (value %in% encountered_values) stop("not hierarchical")
  is_basal = focal_values == ""
  if (is_basal) return(value)
  encountered_values = append(encountered_values, value)
  Recall(focal_values
    , lookup
    , hierarchical_variable
    , nesting_variable
    , encountered_values
  )
}


check_multiple_focal_values = function(value, focal, var) {
  if (length(focal) > 1L) {
    sprintf("%s is nested within multiple values of %s"
      , value
      , var
    ) |> msg() |> stop()
  }
  invisible(NULL)
}
check_missing_nodes = function(value, values, var) {
  is_tree_missing_nodes = length(values) == 0L
  if (is_tree_missing_nodes) {
    sprintf("missing tree nodes. check that %s is included in the %s column"
      , value, var
    ) |> msg() |> stop()
  }
  invisible(NULL)
}

get_focal_values = function(value, lookup, var, nesting) {
  focal_rows = lookup[[var]] == value
  if (!any(focal_rows)) {
    sprintf("value, %s, not found in column, %s", value, var) |> msg() |> stop()
  }
  focal_values = lookup[[nesting]][focal_rows]
  return(focal_values)
}




#' @export
HierarchyNormalizer = function(
       hierarchy_table
     , basal_diseases_to_prune = character()
     , find_unaccounted_cases = TRUE
     , specials_pattern = "_unaccounted$"
  ) {
  function(data
      , hierarchical_variable = NULL
      , basal_variable = NULL
      , nesting_variable = NULL
      , grouping_columns = NULL
    ) {
    data = resolve_hier_args(data, hierarchical_variable)
    hierarchical_columns = c(
        hierarchical_variable
      , basal_variable
      , nesting_variable
    )
    grouping_columns = guess_grouping_columns(
        grouping_columns
      , data
      , hierarchical_columns
    )
  
    hierarchy_table = (hierarchy_table
      |> select(any_of(hierarchical_columns))
      |> distinct()
    )
    
    if (find_unaccounted_cases) data = find_unaccounted_cases(data)
    
    if (!is.null(specials_pattern)) {
      specials = (data
        |> filter(grepl(specials_pattern, disease))
        |> select(disease, nesting_disease)
        |> distinct()
      )
      disease_lookup = bind_rows(disease_lookup, specials)
    }
    pruned_lookup = (disease_lookup
       |> filter(!disease %in% basal_diseases_to_prune)
       |> mutate(nesting_disease = ifelse(
           nesting_disease %in% basal_diseases_to_prune
           , ''
           , nesting_disease
         )
       )
    )
    paths = precompute_paths(pruned_lookup)
    
    if (is.null(grouping_columns)) {
      ## TODO
    }
  
    (data
  
        # prune basal_diseases
        |> mutate(x = disease %in% basal_diseases_to_prune)
        |> mutate(y = nesting_disease %in% basal_diseases_to_prune)
        |> mutate(z = basal_disease %in% basal_diseases_to_prune)
  
        |> filter(!x)
        |> mutate(nesting_disease = ifelse(y, "", nesting_disease))
        |> rowwise()
        |> mutate(basal_disease = ifelse(z, basal_disease(disease, pruned_lookup), basal_disease))
        |> ungroup()
  
        # keeping only leaf diseases
        |> group_by(across(c("basal_disease", all_of(grouping_columns))))
        #filter(is_leaf_disease(disease, nesting_disease))
        |> filter(disease %in% find_leaves_with_precomputed(disease, paths))
        |> ungroup()
  
        # if there is only the basal disease (no sub-diseases), differentiate by adding '-only'
        # mutate(disease = ifelse(disease == basal_disease, sprintf("%s-only", disease), disease))
        # mutate(nesting_disease = basal_disease)
        |> select(-x, -y, -z)
  
    )
  }
}

#' @export
HierarchicalBalancer = function() function(data
      , hierarchical_variable = NULL
      , basal_variable = NULL
      , nesting_variable = NULL
      , grouping_columns = NULL
  ) {
  data = resolve_hier_args(data, hierarchical_variable)
  
  ## example grouping columns:
  ## iso_3166, iso_3166_2, period_start_date, period_end_date,
  ## nesting_disease, source_id, cases_this_period
  
  ## need to do this outside of this function now
  ## data = mutate(data, source_id = source_from_digitization_id(digitization_id))
  
  # check if sum of leaf diseases = reported sum of basal disease
  sum_of_leaf = (
    data
    |> filter(.data[[hierarchical_variable]] != .data[[nesting_variable]])
    |> group_by(iso_3166, iso_3166_2, period_start_date, period_end_date, nesting_disease, basal_disease)
    |> filter(!disease %in% unique(nesting_disease))
    |> mutate(cases_this_period = sum(as.numeric(cases_this_period)))
    |> ungroup()
    |> distinct(iso_3166, iso_3166_2, period_start_date, period_end_date,
                nesting_disease, source_id, cases_this_period)
  )

  reported_totals = (
    data
    |> filter(nesting_disease == '' | is.na(nesting_disease))
    |> filter(disease %in% sum_of_leaf$nesting_disease)
    |> select(-nesting_disease)
    |> rename(nesting_disease = disease)
  )

  # if sum of leaf diseases is < reported sum of basal disease,
  # make new sub-disease called 'disease-name'_unaccounted, which contains
  # the difference between sum of leaf diseases and the reported sum of the disease
  unaccounted_data =
    (inner_join(sum_of_leaf, reported_totals, by =
                  c('iso_3166', 'iso_3166_2', 'period_start_date',
                    'period_end_date', 'nesting_disease', 'original_dataset_id'),
                suffix = c('_sum', '_reported'))

     |> mutate(cases_this_period_reported = as.numeric(cases_this_period_reported),
                cases_this_period_sum = as.numeric(cases_this_period_sum))

     |> filter(cases_this_period_sum < cases_this_period_reported)
     |> mutate(cases_this_period = cases_this_period_reported - cases_this_period_sum)
     |> mutate(disease = paste(nesting_disease, 'unaccounted', sep = '_'))

     |> select(-cases_this_period_reported, -cases_this_period_sum)

     |> mutate(original_dataset_id = '',
               historical_disease = '',
               dataset_id = '')
     |> mutate(record_origin = 'derived-unaccounted-cases')
    )

  if(!"record_origin" %in% names(data)) data = mutate(data, record_origin = 'historical')

  (data
    |> rbind(unaccounted_data)
  )
}
