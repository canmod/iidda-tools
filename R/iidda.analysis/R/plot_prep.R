## Functions that take a data set and return a data set

## Data prep constructors
## ---------------------------------------------------

#' Data Prep Constructors
#'
#' @family data_prep_constructors
#' @name data_prep_constructors
NULL

#' Handle Missing Values in Series Variable
#'
#' Remove or replace values that are \code{NA}.
#'
#' @param na_remove boolean value, if `TRUE` remove `NA`s in series variable
#' @param na_replace numeric value to replace `NA`s in series variable, if NULL
#' no replacement is performed
#'
#' @importFrom dplyr all_of
#' @return a function to remove or replace missing values.
#'
#' ## Returned Function
#'
#' - Arguments
#'    * `data` data frame containing time series data
#'    * `series_variable` column name of series variable in `data`, default is "deaths"
#' - Return - all fields in `data` with either `NA` records removed or replaced
#'
#' @family data_prep_constructors
#' @export
HandleMissingValues <- function(
                           na_remove=FALSE,
                           na_replace=NULL
){

  function(data,
           series_variable="deaths"){
    # remove NAs
    if(na_remove){
      data <- filter(data, !is.na(get(series_variable)))
    }

    # replace NAs
    if (!is.null(na_replace) & length(data[which(is.na(data[series_variable])),][series_variable])!=0){
      data <- data %>% mutate(across(all_of(series_variable),~replace(.,is.na(.),na_replace)))
    }

    return(data)
  }
}



#' Handle Zero Values in Series Variable
#'
#' Remove or replace series variable values that are zero.
#'
#' @param zero_remove boolean value, if `TRUE` remove zeroes in series variable
#' @param zero_replace numeric value to replace zeroes in series variable, if NULL
#' no replacement is performed
#'
#' @return a function to remove or replace zero values.
#'
#' ## Returned Function
#'
#' - Arguments
#'    * `data` data frame containing time series data
#'    * `series_variable` column name of series variable in `data`, default is "deaths"
#' - Return - all fields in `data` with either zero records removed or replaced
#'
#' @family data_prep_constructors
#' @export
HandleZeroValues <- function(
                        zero_remove=FALSE,
                        zero_replace=NULL
                        ){
  function(data,
           series_variable="deaths"){
    # remove zeroes
    if (zero_remove){
      data <- filter(data, get(series_variable) != 0)
    }

    # replace zeroes
    if(!is.null(zero_replace) & length(data[which(data[series_variable]==0),][series_variable])!=0){
      #data[which(data$y==0),]$y <- zero_replace
      data <- data %>% mutate(across(all_of(series_variable),~replace(.,. == 0,zero_replace)))

    }
    return(data)
  }
}


#' Trim Time Series
#'
#' Remove leading or trailing zeroes in a time series data set.
#'
#' @param zero_lead boolean value, if `TRUE` remove leading zeroes in `data`
#' @param zero_trail boolean value, if `TRUE` remove trailing zeroes in `data`
#'
#' @importFrom dplyr arrange pull
#' @return a function to remove to remove leading and/or trailing zeroes
#'
#' ## Returned Function
#'
#' - Arguments
#'    * `data` data frame containing time series data
#'    * `series_variable` column name of series variable in `data`, default is "deaths"
#'    * `time_variable` column name of time variable in `data`, default is "period_start_date"
#' - Return - all fields in `data` with filtered records to trim leading and/or trailing zeroes
#'
#' @family data_prep_constructors
#' @export
TrimSeries <- function(zero_lead=FALSE,
                       zero_trail=FALSE){
  function(data,
           series_variable="deaths",
           time_variable="period_start_date"){
    # trim leading zeroes
    if (zero_lead){
      first_date <- (data
                     %>% filter(get(series_variable) !=0)
                     %>% arrange(get(time_variable))
                     %>% filter(row_number()==1)
                     %>% select(all_of(time_variable))
                     %>% pull()

      )
      data <- filter(data, get(time_variable) >= first_date)
    }

    # trim trailing zeroes
    if (zero_trail){
      last_date <- (data
                    %>% filter(get(series_variable) !=0)
                    %>% arrange(get(time_variable))
                    %>% filter(row_number()==n())
                    %>% select(all_of(time_variable))
                    %>% pull()

      )
      data <- filter(data, get(time_variable) <= last_date)
    }
    return(data)
  }
}

#' Wavelet Series Harmonizer
#'
#' Harmonizes the series variable in `data` so there is one data value for each time
#' unit in time variable (to account for different variations in disease/cause name)
#'
#' @param time_variable column name of time variable in `data`, default is "period_start_date"
#' @param series_variable column name of series variable in `data`, default is "deaths"
#'
#' @importFrom dplyr group_by summarize ungroup
#' @return function to harmonize disease/cause names
#'
#' ## Returned Function
#'
#' - Arguments
#'    * `data` data frame containing time series data
#' - Return - all fields in `data` with summarized series variable for unique time variable
#'
#' @family data_prep_constructors
#' @export
SeriesHarmonizer = function(
    time_variable = "period_start_date",
    series_variable = "deaths"
) {
  function(data) {
    (data
     %>% group_by(across(time_variable))
     %>% summarize(!!sym(series_variable) := sum(get(series_variable)))
     %>% ungroup()
    )
  }

}

#' De-heaping time series
#'
#' Fixes heaping errors in time series. The structure of this function was taken from
#' the function `find_heap_and_deheap` created by Kevin Zhao
#' (https://github.com/davidearn/KevinZhao/blob/main/Report/make_SF_RData.R). This needs
#' to be better documented.
#'
#' @param time_variable column name of time variable in `data`, default is "period_start_date"
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param first_date string containing earliest date to look for heaping errors
#' @param last_date string containing last date to look for heaping errors
#' @param week_start numeric value of the first week number to start looking for heaping errors
#' @param week_end numeric value of the last week number to look for heaping errors
#'
#' @importFrom dplyr if_else group_split lag lead
#' @return function to fix heaping errors
#'
#' ## Returned Function
#'
#' - Arguments
#'    * `data` data frame containing time series data
#' - Return - all fields in `data` with an additional field called "deheaped_" concatenated with `series_variable`.
#' If no heaping errors are found, this additional field is identical to the field `series_variable
#'
#' @family data_prep_constructors
#' @export
WaveletDeheaper = function(
    time_variable = "period_start_date",
    series_variable = "deaths",
    first_date = "1830-01-01",
    last_date = "1841-12-31",
    week_start = 45,
    week_end = 5
    # add 2.7 parameter
) {
  function(data) {

    # filter data by time period and relevant weeks, create grouping variable to reflect year ends
    filter_data <- (data
                    %>% filter(get(time_variable) >= first_date & get(time_variable) <= last_date)
                    %>% filter(week(get(time_variable)) >= week_start | week(get(time_variable)) <= week_end)
                    %>% mutate(group_var = if_else(week(get(time_variable)) >= week_start,
                                                   year(get(time_variable)),
                                                   year(get(time_variable))-1)
                    )
                    %>% group_by(group_var)
                    %>% group_split()
    )

    if (length(filter_data)!=0){
      final_data <- lapply(seq_along(filter_data),function(i){

        # identify potential heap value and compute updated value by averaging previous and post data
        intermediate_data <- (filter_data[[i]]
                              %>% mutate(stdev=sd(get(series_variable)),med=median(get(series_variable)))
                              # this heaping threshold was taking directly from `find_heap_and_deheap`
                              %>% mutate(heap=if_else((get(series_variable)-med >= 2.7*stdev),TRUE,FALSE))
                              %>% arrange(get(time_variable))
                              %>% mutate(updated_series = if_else(heap==TRUE,
                                                                  (lag(get(series_variable))+lead(get(series_variable)))/2,
                                                                  get(series_variable)))
        )
        # get old value of heaped data point
        old <- intermediate_data[[series_variable]][intermediate_data$heap==TRUE]

        if (length(old)==1){

          # get new value of heaped data point
          new <- intermediate_data$updated_series[intermediate_data$heap==TRUE]

          # compute deheaped series
          intermediate_data <- (intermediate_data
                                # this computation to create the deheaped field was taken directly from `find_heap_and_deheap`
                                %>% mutate(!!sym(paste0("deheaped_",series_variable)) := updated_series+((old-new)*updated_series)/sum(updated_series))
                                %>% select(time_variable, paste0("deheaped_",series_variable))
                                # %>% mutate(!!series_variable := updated_series+((old-new)*updated_series)/sum(updated_series))
                                # %>% select(time_variable, series_variable)
          )
        } else {
          # set deheaped series to current series
          # TODO Should find a way to return no deheaped series if it is not needed
          intermediate_data <- (intermediate_data
                                %>% mutate(!!sym(paste0("deheaped_",series_variable)) := get(series_variable))
                                %>% select(time_variable, paste0("deheaped_",series_variable))
                                # %>% mutate(!!series_variable := get(series_variable))
                                # %>% select(time_variable, series_variable)
          )
        }
        intermediate_data
      }) %>% bind_rows()
      #browser()
      # add deheaped series to data set
      data <- (data
               %>% left_join(final_data,by=time_variable)
      )
      #data <- final_data
    }

    return(data)
  }

}


#' Wavelet Joiner
#'
#' Joins series data and trend datasets and keeps all time units in one of the
#' datasets.
#'
#' @param time_variable column name of time variable in `data`, default is "period_start_date"
#' @param series_suffix suffix to be appended to series data fields
#' @param trend_suffix suffix to be appended to trend data fields
#' @param keep_series_dates boolean flag to indicate if the dates in `series_data` should
#' be kept and data from `trend_data` is left joined, if `FALSE` dates from `trend_data`
#' are left joined instead
#'
#' @return function to join data and trend data sets
#'
#' ## Returned Function
#'
#' - Arguments
#'    * `series_data` data frame containing time series data
#'    * `trend_data` data frame containing trend data
#' - Return - joined data set by `time_variable` with updated field names
#'
#' @family data_prep_constructors
#' @export
WaveletJoiner = function(
    time_variable = "period_start_date",
    series_suffix = "_series",
    trend_suffix = "_trend",
    keep_series_dates = TRUE
) {
  function(series_data, trend_data) {
    if (keep_series_dates)
    {
      (series_data
       %>% left_join(trend_data,by=time_variable, suffix=c(series_suffix, trend_suffix))
      )
    } else {
      (trend_data
       %>% left_join(series_data,by=time_variable, suffix=c(trend_suffix, series_suffix))
      )
    }
  }
}


#' Wavelet Interpolator
#'
#' Linearly interpolates `NA` values in both series and trend variables.
#'
#' @param series_variable column name of series variable in `data`, default is "deaths_series"
#' @param trend_variable column name of series variable in `data`, default is "deaths_trend"
#' @param time_variable column name of time variable in `data`, default is "period_start_date"
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
#' @family data_prep_constructors
#' @export
WaveletInterpolator = function(
    time_variable = "period_start_date",
    series_variable = "deaths",
    trend_variable = "deaths",
    series_suffix = "_series",
    trend_suffix = "_trend"
) {
  function(data) {

    # form field names
    series_variable = paste0(series_variable, series_suffix)
    trend_variable = paste0(trend_variable, trend_suffix)

    # filter out NA values
    series_data <- (data
                    %>% filter(!is.na(get(series_variable)))
                    %>% select(time_variable, series_variable)
    )
    trend_data <- (data
                   %>% filter(!is.na(get(trend_variable)))
                   %>% select(time_variable, trend_variable)
    )

    # identify dates with NA values in series and trend variables
    series_dates <- (data
                     %>% filter(is.na(get(series_variable)))
                     %>% select(time_variable)
                     %>% pull
    )
    trend_dates <- (data
                    %>% filter(is.na(get(trend_variable)))
                    %>% select(time_variable)
                    %>% pull
    )

    # interpolate at time point corresponding to NA values in
    # series and trend variables
    series_interp <- (approx(x=series_data[[time_variable]],
                             y=series_data[[series_variable]],
                             xout=series_dates,
                             method="linear")
                      %>% data.frame()
                      %>% setNames(c(time_variable,series_variable))
    )

    trend_interp <- (approx(x=trend_data[[time_variable]],
                            y=trend_data[[trend_variable]],
                            xout=trend_dates,
                            method="linear")
                     %>% data.frame()
                     %>% setNames(c(time_variable,trend_variable))
    )
    # combined final data set
    (data
      %>% select(-series_variable, -trend_variable)
      %>% left_join((series_data %>% union(series_interp)),by=time_variable)
      %>% left_join((trend_data %>% union(trend_interp)),by=time_variable)
      # remove leading and trailing NAs (that can't be interpolated)
      %>% filter(!is.na(get(series_variable)),!is.na(get(trend_variable)))
    )

  }
}


#' Wavelet Normalizer
#'
#' Creates normalizing fields in `data`
#'
#' @param time_variable column name of time variable in `data`, default is "period_start_date"
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
#' @family data_prep_constructors
#' @export
WaveletNormalizer = function(
    time_variable = "period_start_date",
    series_variable = "deaths",
    trend_variable = "deaths",
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
  function(data) {

    # form field names
    series_variable = paste0(series_variable, series_suffix)
    trend_variable = paste0(trend_variable, trend_suffix)


    (data
     %>% arrange(get(time_variable))
     # can't seem to use a date variable for tt, using row index instead
     %>% mutate(!!output_emd_trend := emd(xt = data[[trend_variable]],
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
  }

}

#' Wavelet Transformer
#'
#' Compute the wavelet transform.
#'
#' @param time_variable name of the time variable field in `data`
#' @param wavelet_variable name of the field in `data` to be wavelet transformed
#'
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
#' @family data_prep_constructors
#' @export
WaveletTransformer = function(
    time_variable = "period_start_date",
    wavelet_variable = "detrend_norm",
    #all arguments to analyze.wavelet
    ...

) {
  function(data) {
    analyze_data <- (data %>% rename("date"=time_variable))
    analyze.wavelet(my.data=analyze_data,
                    my.series=wavelet_variable,
                    ...
    )
  }
}



#' Compute Moving Average of Time Series
#'
#' @param ma_window_length length of moving average window, this will depend on the time scale in the data.
#' Defaults to 52, so that weekly data is averaged over years.
#'
#' @return a function to remove to compute the moving average of a time series variable
#'
#' ## Returned Function
#'
#' - Arguments
#'    * `data` data frame containing time series data
#'    * `series_variable` column name of series variable in `data`, default is "deaths"
#'    * `time_variable` column name of time variable in `data`, default is "period_start_date"
#' - Return - all fields in `data` with the `series_variable` data replaced with the moving
#' average.
#'
#' @family data_prep_constructors
#' @export
ComputeMovingAverage <- function(ma_window_length=52){

  function(data,
           series_variable="deaths",
           time_variable="period_start_date"){

    # compute moving average
    ma_filter <- rep(1,ma_window_length)/ma_window_length

    data <- data %>% mutate(across(all_of(series_variable), ~stats::filter(.,ma_filter)))
    #data$y <- stats::filter(data$y,ma_filter)

    return(data)
  }
}

# ------------------------------------
# prep functions TODO: better name?

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
#' @param time_variable column name of time variable in `x` and `y`, default is "period_start_date"
#'
#' @importFrom janitor compare_df_cols_same
#' @return combined `x` and `y` data frames with optional filtering for overlaps
#'
#' @export
union_series <- function(x,
                         y,
                         overlap = TRUE,
                         time_variable="period_start_date"){

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
#' heatmap, however it might make sense to do something sensible like dividing the series variable value in half and allocating
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
                 %>% filter(get(start_year_variable)!= get(end_year_variable))
                 %>% pivot_longer(cols=c(start_year_variable,end_year_variable),names_to=temp_year_variable,values_to="value")
                 %>% mutate(!!end_day_variable := if_else(value %% 4 !=0 & get(temp_year_variable)==start_year_variable, 365, get(end_day_variable)),
                            !!end_day_variable := if_else(value %% 4 ==0 & get(temp_year_variable)==start_year_variable, 366, get(end_day_variable)))
                 %>% mutate(!!start_day_variable := if_else(get(temp_year_variable)== end_year_variable, 0, get(start_day_variable)))
                 %>% rename(!!start_year_variable:=value)
                 # create temp end year field, is this really needed?
                 %>% mutate(!!end_year_variable := get(start_year_variable))
                 %>% select(-all_of(temp_year_variable))
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
                      axisTicks(log(range(x+1, na.rm = TRUE)), log = TRUE, n = n)
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

#' Prep Data for Time Series
#'
#' Prep data for basic time series plot. Prep steps were taken from `LBoM::plot.LBoM` and they include
#' handling missing values and zeroes, and optionally trimming time series.
#'
#' @param data data frame containing time series data
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param time_variable column name of time variable in `data`, default is "period_start_date"
#' @param grouping_variable column name of the grouping variable in `data` to summarize the series variable over,
#' if `summarize=TRUE`
#' @param time_unit time unit to sum series data over, must be one of plot_vars.R time_units, defaults to "year".
#' @param summarize_series boolean value to indicate summarizing by `time_unit` over the series variable
#' @param trim_zeroes boolean value to filter data to exclude leading and trailing zeroes
#' @param trim_series function to trim leading and trailing series zeroes, defaults to TrimSeries
#' @param handle_missing_values function to handle missing values, defaults to HandleMissingValues
#' @param handle_zero_values function to handle zero values, defaults to HandleZeroValues
#'
#' @importFrom dplyr group_by_at
#' @return all fields in `data` with records prepped for plotting moving average time series
#'
#' @family prep_data_for_plotting
#' @export
iidda_prep_series <- function(data,
                         series_variable="deaths",
                         time_variable="period_start_date",
                         grouping_variable="cause",# not sure if I need this
                         time_unit = "year",
                         summarize_series = TRUE,
                         trim_zeroes=TRUE,
                         trim_series=TrimSeries(zero_lead=FALSE,zero_trail=FALSE),
                         handle_missing_values  = HandleMissingValues(na_remove = FALSE, na_replace = NULL),
                         handle_zero_values = HandleZeroValues(zero_remove = FALSE, zero_replace = NULL)){

  missing_value_handled_data <- handle_missing_values(data,series_variable=series_variable)
  zero_handled_data <- handle_zero_values(missing_value_handled_data,series_variable=series_variable)

  if (trim_zeroes){
    series_data <- trim_series(zero_handled_data,series_variable=series_variable, time_variable=time_variable)
  } else  {
    series_data <- zero_handled_data
  }

  if (summarize_series){
    series_data <- (mutate_time_vars(series_data,unit=time_unit)
                 %>% group_by_at(vars(get_unit_labels(time_unit)))
                 # overwrite series variable for plotting?
                 %>% summarize(!!series_variable := sum(get(series_variable), na.rm=TRUE))
    )
  }
  return(series_data)
}


#' Prep Data for Moving Average Plot
#'
#' Prep data for plotting moving average. Prep steps were taken from `LBoM::plot.LBoM` and they include
#' handling missing values and zeroes, optionally trimming time series and computing the moving
#' average.
#'
#' @param data data frame containing time series data
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param time_variable column name of time variable in `data`, default is "period_start_date"
#' @param trim_zeroes boolean value to filter data to exclude leading and trailing zeroes
#' @param trim_series function to trim leading and trailing series zeroes, defaults to TrimSeries
#' @param handle_missing_values function to handle missing values, defaults to HandleMissingValues
#' @param handle_zero_values function to handle zero values, defaults to HandleZeroValues
#' @param compute_moving_average function to compute the moving average of `series_variable`
#'
#'
#' @return all fields in `data` with records prepped for plotting moving average time series
#'
#' @family prep_data_for_plotting
#' @export
iidda_prep_ma <- function(data,
                         series_variable="deaths",
                         time_variable="period_start_date",
                         trim_zeroes=TRUE,
                         trim_series=TrimSeries(zero_lead=FALSE,zero_trail=FALSE),
                         handle_missing_values  = HandleMissingValues(na_remove = FALSE, na_replace = NULL),
                         handle_zero_values = HandleZeroValues(zero_remove = FALSE, zero_replace = NULL),
                         compute_moving_average = ComputeMovingAverage(ma_window_length = 52)){

  missing_value_handled_data <- handle_missing_values(data,series_variable=series_variable)
  zero_handled_data <- handle_zero_values(missing_value_handled_data,series_variable=series_variable)

  if (trim_zeroes){
    trimmed_data <- trim_series(zero_handled_data,series_variable=series_variable, time_variable=time_variable)
    ma_data <- compute_moving_average(trimmed_data,series_variable=series_variable, time_variable=time_variable)
  } else {
    ma_data <- compute_moving_average(zero_handled_data,series_variable=series_variable, time_variable=time_variable)
  }
  return(ma_data)
}

#' Prep Data for Bar Graph
#'
#' Prep data for plotting bar graphs. Prep steps were taken from `LBoM::monthly_bar_graph` and `LBoM::weekly_bar_graph`
#' and they include handling missing values and aggregating series data by time unit grouping variable.
#'
#' @param data data frame containing time series data
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param time_variable column name of time variable in `data`, default is "period_start_date"
#' @param time_unit time unit to sum series data over, must be one of plot_vars.R time_units, defaults to "week".
#' @param handle_missing_values function to handle missing values, defaults to HandleMissingValues
#' @param handle_zero_values function to handle zero values, defaults to HandleZeroValues
#'
#'
#' @return `data` with records prepped for plotting bar graphs with `series_variable` and `time_unit` field. The name
#' of the resulting `time_unit` field will be named from lubridate_funcs.
#'
#' @family prep_data_for_plotting
#' @export
iidda_prep_bar <- function(data,
                          series_variable="deaths",
                          time_variable = "period_start_date",
                          time_unit = "week", #has to be one of plot_vars.R time_units
                          handle_missing_values  = HandleMissingValues(na_remove = FALSE, na_replace = NULL),
                          handle_zero_values = HandleZeroValues(zero_remove = FALSE, zero_replace = NULL)){

  missing_value_handled_data <- handle_missing_values(data,series_variable=series_variable)
  zero_handled_data <- handle_zero_values(missing_value_handled_data,series_variable=series_variable)

  # create time_unit variable and summarize series_variable
  bar_data <- (mutate_time_vars(zero_handled_data,unit=time_unit)
                       %>% group_by_at(vars(get_unit_labels(time_unit)))
                       # overwrite series variable for plotting?
                       %>% summarize(!!series_variable := sum(get(series_variable), na.rm=TRUE))
  )

  return(bar_data)
}

#' Prep Data for Box plot
#'
#' Prep data for plotting box plots. Prep steps were taken from `LBoM::monthly_box_plot`
#' and they include handling missing values and creating additional time unit fields.
#'
#' @param data data frame containing time series data
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param time_variable column name of time variable in `data`, default is "period_start_date"
#' @param time_unit time unit to create field from `time_variable`. Must be one of plot_vars.R time_units, defaults to "week".
#' @param handle_missing_values function to handle missing values, defaults to HandleMissingValues
#' @param handle_zero_values function to handle zero values, defaults to HandleZeroValues
#'
#'
#' @return all fields in`data` with records prepped for plotting box plots. The name
#' of the new `time_unit` field will be named from lubridate_funcs.
#'
#' @family prep_data_for_plotting
#' @export
iidda_prep_box <- function(data,
                          series_variable="deaths",
                          time_variable = "period_start_date",
                          time_unit = "week", #has to be one of plot_vars.R time_units
                          handle_missing_values  = HandleMissingValues(na_remove = FALSE, na_replace = NULL),
                          handle_zero_values = HandleZeroValues(zero_remove = FALSE, zero_replace = NULL)){

  missing_value_handled_data <- handle_missing_values(data,series_variable=series_variable)
  zero_handled_data <- handle_zero_values(missing_value_handled_data,series_variable=series_variable)

  # create time_unit variable
  box_data <- (mutate_time_vars(missing_value_handled_data,unit=time_unit)
               %>% mutate(get_unit_labels(time_unit)) ##make it a factor variable
  )

  return(box_data)
}

#' Prep Data for Heatmap
#'
#' Prep data for heatmap plots. Prep steps were taken from `LBoM::seasonal_heat_map`
#' and they include creating additional time unit fields, splitting weeks that cover the
#' year end,  and optionally normalizing series data to be in the range (0,1).
#'
#' @param data data frame containing time series data
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param start_time_variable column name of time variable in `data`, default is "period_start_date"
#' @param end_time_variable column name of time variable in `data`, default is "period_end_date"
#' @param time_unit a vector of new time unit fields to create from `start_time_variable` and `end_time_variable`.
#' Defaults to "c("yday","year")". The currently functionality expects that both "yday" and "year" are included, should be
#' made more general to incorporate any of plot_vars.R time_units.
#' @param prepend_string string to prepend to newly created time_unit fields to distinguish between time_unit
#' fields corresponding to starting versus ending time periods. Defaults to "End ". For example, a `time_unit` of "year"
#' will create a field name "Year" from `start_time_variable` and a field called "End Year" created from `end_time_variable`.
#' @param normalize boolean flag to normalize `series_variable` data to be between 0 and 1.
#' @param ... optional arguments to `year_end_fix()`
#'
#' @importFrom purrr map reduce map2
#' @importFrom scales rescale
#' @return all fields in`data` with records prepped for plotting heatmaps. The name
#' of the new `time_unit` fields will be named from lubridate_funcs.
#'
#' @family prep_data_for_plotting
#' @export
iidda_prep_heatmap <- function(data,
                          series_variable="deaths",
                          start_time_variable = "period_start_date",
                          end_time_variable = "period_end_date",
                          time_unit = c("yday","year"), #has to be one of plot_vars.R time_units
                          prepend_string = "End ",
                          normalize = FALSE,
                          ...){


  # add starting time unit variables
  add_starting_time <- (map(time_unit, ~ mutate_time_vars(data,input_nm=start_time_variable,unit=.x))
                        %>% reduce(left_join, by=colnames(data))
  )

  # add ending time unit variables
  # prepend "End" to each of time_unit unit_labels to prevent duplicate names
  add_ending_time <- (map2(time_unit ,paste0(prepend_string, get_unit_labels(time_unit)), ~ mutate_time_vars(data,input_nm=end_time_variable,unit=.x,output_nm=.y))
                      %>% reduce(left_join, by=colnames(data))
  )

  # combine data containing all new time column names
  new_data <- (add_starting_time
                %>% left_join(add_ending_time, by=colnames(data))
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
                   %>% filter(get(start_year_name)==get(end_year_name))
                   %>% union(end_of_year_data)
                   %>% group_by(across(start_year_name))
                   %>% {if (normalize) mutate(.,!!series_variable := scales::rescale(get(series_variable),to=c(0,1))) else .}
                   %>% ungroup()
  )

  return(heat_data)
}


#' Prep Data for Rohani Plot
#'
#' Prep data for rohani plots. Prep steps include creating additional time unit fields, summarizing the series
#' variable by time unit and grouping variable (the x and y axis variables) ,and optionally normalizing series
#' data to be in the range (0,1). By default, the grouping variable is ranked in order of the summarized series
#' variable. Needs to be generalized more, might
#' need to handle the case where the desired y-axis is a second time unit, as in the heatmap plot and therefore
#' making use of the year_end_fix function.
#'
#' @param data data frame containing time series data
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param time_variable column name of time variable in `data`, default is "period_start_date"
#' @param start_time_variable column name of time variable in `data`, default is "period_start_date"
#' @param time_unit a vector of new time unit fields to create from `start_time_variable` and `end_time_variable`.
#' Defaults to "c("year")". The currently functionality expects that "year" is included, should be
#' made more general to incorporate any of plot_vars.R time_units.
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
#' @family prep_data_for_plotting
#' @export
iidda_prep_rohani <- function(data,
                              series_variable="deaths",
                              time_variable = "period_start_date",
                              start_time_variable = "period_start_date",
                              #end_time_variable = "period_end_date", # might not need this we are plotting a second time unit on the y-axis
                              time_unit = c("year"), #has to be one of plot_vars.R time_units
                              grouping_variable = "cause",
                              ranking_variable = NULL , #optionally specify the ranking variable to order by?
                              #prepend_string = "End ", # might not need this
                              normalize = FALSE,
                              handle_missing_values  = HandleMissingValues(na_remove = FALSE, na_replace = NULL),
                              handle_zero_values = HandleZeroValues(zero_remove = FALSE, zero_replace = NULL),
                              create_nonexistent=FALSE){

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
                        %>% reduce(left_join, by=colnames(zero_handled_data))
  )

  # assumes one of time_unit=="year", this should be generalized somehow
  start_year_name <- grep("^(?=.*Year)(?:(?!Day).)*$",colnames(add_time_units),perl=TRUE,value=TRUE)


  if(create_nonexistent){

    # get unique grouping variable and time unit variable
    grouping_time_combinations <- expand.grid(unique(add_time_units[[grouping_variable]]),
                                              unique(add_time_units[[start_year_name]])) %>% setNames(c(grouping_variable,start_year_name))

    # create all records
    add_time_units <- (grouping_time_combinations
                       %>% left_join(add_time_units))
  }


  # how should data be ranked, by default rank data by summarized series variable for each group
  ranking_col <- (add_time_units
                  %>% group_by_at(vars(grouping_variable))
                  #to get the ranking group
                  %>% summarize(summarized_series_variable = if_else(all(is.na(get(series_variable))), NA_real_, sum(get(series_variable), na.rm = TRUE)))
                  %>% arrange(desc(summarized_series_variable))
                  %>% select(matches(grouping_variable))
                  %>% pull()
  )



  rohani_data <- (add_time_units
                  %>% group_by_at(vars(start_year_name,grouping_variable))
                #%>% summarize(grouped_deaths = sum(deaths))
                %>% dplyr::summarize(!!series_variable := if_else(all(is.na(get(series_variable))), NA_real_, sum(get(series_variable), na.rm = TRUE)))
                # if grouping variable is categorical?
                %>% {if (normalize) mutate(.,!!series_variable := scales::rescale(get(series_variable),to=c(0,1))) else .}
                %>% mutate(!!grouping_variable := factor(get(grouping_variable), levels=ranking_col))
                %>% arrange(desc(get(grouping_variable))) # not sure if desc is required
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
#' @param time_variable column name of time variable in `data`, default is "period_start_date"
#' @param time_unit time unit to create field from `time_variable`. Must be one of plot_vars.R time_units, defaults to "week".
#' @param handle_missing_values function to handle missing values, defaults to HandleMissingValues
#' @param handle_zero_values function to handle zero values, defaults to HandleZeroValues
#'
#'
#' @importFrom stats spec.pgram
#' @return all fields in`data` with records prepped for plotting box plots. The name
#' of the new `time_unit` field will be named from lubridate_funcs.
#'
#' @family prep_data_for_plotting
#' @export
iidda_prep_periodogram <- function(data,
                                  series_variable="deaths",
                                  time_variable = "period_start_date",
                                  transform = FALSE,
                                  transformation = "log10",
                                  spans = NULL,
                                  kernel = NULL,
                                  taper = 0.1,
                                  pad = 0,
                                  fast = TRUE,
                                  demean = FALSE,
                                  detrend = TRUE,
                                  na.action = na.fail,
                                  # normalize spectrum to [0,1]
                                  normalize = TRUE,
                                  # time periods in a year (52 weeks in a year), do we need to account for other time units
                                  periods_per_year = 52,
                                  handle_missing_values  = HandleMissingValues(na_remove = TRUE, na_replace = NULL)){

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
                   %>% data.frame()
                   %>% filter(per <= max_period)
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
#' @param time_variable column name of time variable in `data`, default is "period_start_date"
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
#' @family prep_data_for_plotting
#' @export
iidda_prep_wavelet = function(
    data,
    trend_data,
    time_variable = "period_end_date",
    series_variable = "deaths",
    trend_variable = "deaths",
    series_suffix = "_series",
    trend_suffix = "_trend",
    wavelet_variable = "detrend_norm",
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
    data_harmonizer = SeriesHarmonizer(time_variable, series_variable),
    trend_data_harmonizer = SeriesHarmonizer(time_variable, trend_variable),
    data_deheaper = WaveletDeheaper(time_variable, series_variable),
    trend_deheaper = WaveletDeheaper(time_variable, trend_variable),
    joiner = WaveletJoiner(time_variable, series_suffix, trend_suffix),
    interpolator = WaveletInterpolator(time_variable, series_variable, trend_variable, series_suffix, trend_suffix),
    normalizer = WaveletNormalizer(time_variable,
                                   series_variable,
                                   trend_variable,
                                   series_suffix,
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
                                   output_detrend_log),
    transformer = WaveletTransformer(time_variable,
                                     wavelet_variable,
                                     dt = 1/52,
                                      dj = 1/50,
                                      lowerPeriod = 1/2,
                                      upperPeriod = 10,
                                      n.sim=1000,
                                      make.pval = TRUE,
                                      date.format = "%Y-%m-%d")
) {
  series_harmonized = data_harmonizer(data)
  trend_harmonized = trend_data_harmonizer(trend_data)
  series_deheaped = data_deheaper(series_harmonized)
  trend_deheaped = trend_deheaper(trend_harmonized)
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
    %>% mutate(z = c(transformed_data$Power))
    %>% mutate(cont = c(transformed_data$Power.pval))
    %>% mutate(ridge = c(transformed_data$Ridge * transformed_data$Power))
  )

  ## Compute tile height and width for geom_tile
  tile_data_to_plot = (cont_data_to_plot
    %>% arrange(y_loc,x_loc)
    %>% mutate(x_wid = as.numeric(difftime(lead(x_loc), x_loc, units = 'hours')))
    %>% mutate(x_loc = x_loc + dhours(0.5 * x_wid))
    %>% mutate(x_wid = dhours(x_wid))
    %>% arrange(x_loc,y_loc)
    %>% mutate(y_ht = lead(y_loc)-y_loc)
    %>% mutate(y_ht = if_else(y_ht <= 0, min(y_ht[y_ht>0],na.rm=TRUE), y_ht))
    %>% mutate(y_ht = if_else(is.na(y_ht),min(y_ht[y_ht>0],na.rm=TRUE), y_ht))
    %>% mutate(y_loc = y_loc + (0.5*y_ht))
    %>% filter(x_wid > 0)
  )

  return(nlist(transformed_data, tile_data_to_plot, cont_data_to_plot))
}


#' Get LBoM metadata
#'
#' Get starting time period, ending time period and mortality cause name from the
#' data set for use in axis and main plot titles.
#'
#' @param data data frame containing time series data
#' @param time_variable column name of time variable in `data`, default is "period_start_date"
#' @param descriptor_variable column name of the descriptor variable in `data`, default is "cause" for
#' mortality data sets.
#'
#' @return a list in order containing minimum time period, maximum time period and cause name.
#' @export
iidda_get_metadata <- function(data,
                              time_variable = "period_start_date",
                              descriptor_variable = "cause"
                              #time_unit, might want this
                              ){

  min_time <- min(data[,time_variable],na.rm=TRUE)
  max_time <- max(data[,time_variable],na.rm=TRUE)
  descriptor_name <- data %>% select(all_of(descriptor_variable)) %>% unique()

  return(list(min_time=min_time,max_time=max_time,descriptor_name=descriptor_name))
}




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
#' @param time_variable column name of time variable in `data`, default is "period_start_date"
#'
#' @importFrom ggplot2 geom_line scale_y_continuous
#' @return a ggplot2 plot object containing a moving average time series
#' @family plotting_functions
#' @export
iidda_plot_ma <- function(plot_object,
                         data=NULL,
                         series_variable="deaths",
                         time_variable="period_start_date"){

  (plot_object
   + geom_line(data=data, aes(x=.data[[time_variable]],y=.data[[series_variable]]))
   # + labs(x=time_variable,y=series_variable)
   + scale_y_continuous()
  )
}

#' Plot  Time Series
#'
#' Add a time series line to an exiting ggplot plot object.
#'
#' @param plot_object a `ggplot2` plot object
#' @param data data frame containing time series data, typically output from `iidda_prep_series()`.
#' If `NULL` data is inherited from `plot_object`
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param time_variable column name of time variable in `data`, default is "period_start_date"
#' @param time_unit time unit to display on the x-axis.
#'
#' @family plotting_functions
#' @return a ggplot2 plot object containing a moving average time series
iidda_plot_series <- function(plot_object,
                         data=NULL,
                         series_variable="deaths",
                         time_variable="period_start_date",
                         time_unit="year"){

  plot_object + geom_line(data=data, aes(x=.data[[get_unit_labels(time_unit)]],y=.data[[series_variable]]))
}

#' Plot Bar Graph
#'
#' Add a bar plot to an exiting ggplot plot object. Graphical choices were made to closely reflect
#' plots generated with `LBoM::monthly_bar_graph` and `LBoM::weekly_bar_graph`.
#'
#' @param plot_object a `ggplot2` plot object
#' @param data data frame containing data prepped for bar plotting, typically output from `iidda_prep_bar()`.
#' If `NULL` data is inherited from `plot_object`
#' @param series_variable column name of series variable in `data`, default is "deaths"
#' @param time_unit time unit to display bar graphs on the x-axis. Defaults to "week" or one of plot_vars.R time_units that starts
#' with "month". Should generalize at some point to be able to take any time_unit argument.
#'
#' @importFrom grDevices rainbow
#' @importFrom ggplot2 geom_col
#' @return a ggplot2 plot object containing a bar graphs of time series data
#'
#' @family plotting_functions
#' @export
iidda_plot_bar <- function(plot_object,
                          data=NULL,
                          series_variable="deaths",
                          time_unit="week"){

  ## these were the original colours from LBoM (not sure if this is needed going forward)
  if(grepl("^month",time_unit,perl=TRUE)) {
    # colours for plot (taken from LBoM::monthly_bar_graph)
    plot_colours <-  c("coral4", "orangered1", "orange", "yellow",
                               "yellowgreen","forestgreen", "turquoise4",
                               "blue4", "purple1", "magenta3",
                               "deeppink3", "indianred1")
    }else {
      plot_colours <- rainbow(53)
  }

  plot_object + geom_col(data=data, aes(x= .data[[get_unit_labels(time_unit)]], y= .data[[series_variable]]),fill = plot_colours)  #+ theme(axis.text.x = element_text(angle = 70)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 6))
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
#' handle any time_unit from plot_vars.R time_units.
#' @param ... other arguments to be passed to `scale_x_discrete`
#'
#' @importFrom ggplot2 geom_boxplot scale_x_discrete
#' @return a ggplot2 plot object containing a box plots of time series data
#'
#' @family plotting_functions
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
#'
#' @importFrom ggplot2 ggplot_build geom_rect scale_x_continuous scale_y_continuous scale_fill_distiller xlab ylab
#' @importFrom lubridate yday
#' @return a ggplot2 plot object containing a yearly vs. weekly heatmap of time series data
#'
#' @family plotting_functions
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
#' @param colour_trans function to scale colours, to be supplied to trans argument of scale_fill_gradientn()
#' @param n_colours vector of colours to be supplied to scale_fill_gradientn()
#' @param NA_colour colour for `NA` values, defaults to "black"
#' @param palette_colour colour of heatmap palette, defaults to "RdGy". Should specify what type of palette colours
#' are accepted by this argument.
#'
#' @importFrom ggplot2 geom_raster scale_y_discrete scale_fill_gradientn labs theme_bw theme element_blank
#' @return a ggplot2 plot object containing a yearly vs. weekly heatmap of time series data
#'
#' @family plotting_functions
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
#' @family plotting_functions
#' @export
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
#' @family plotting_functions
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
                  %>% full_join(data.frame(cbind(coi=wavelet_data$coi.1,
                                                 y=wavelet_data$coi.2)),
                                by=c("axis_1"="coi"),keep=TRUE
                  )
                  %>% filter(!is.na(date))
                  %>% filter(y >= ymin)
                  %>% filter(y <= ymax)
                  %>% select(-coi,-axis_1)
                  %>% mutate(y=if_else(date==min(date),ymin,y))
                  %>% mutate(y=if_else(date==max(date),ymin,y))
                  %>% union(plot_edges)

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
#' @param time_variable column name of time variable in `data`, default is "period_start_date"
#' @param filter_variable column name of variable to filter on in `data`, default is "period_start_date"
#' @param filter_start value of `filter_variable` for starting range, default is "1700-01-01"
#' @param filter_end value of `filter_variable` for ending range, default is "1800-01-01"
#' @param ... other arguments to be passed to `ggforce::geom_mark_rect`, for example annotating with text
#'
#' @importFrom ggforce geom_mark_rect
#' @return a ggplot2 plot object a rectangular plot highlight
#' @family plotting_functions
#' @export
iidda_plot_highlight <- function(
    plot_object,
    data=NULL,
    series_variable="deaths",
    time_variable="period_start_date",
    filter_variable="period_start_date", # variable to filter on
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
#' @family plotting_functions
#' @export
iidda_plot_scales <- function(
    plot_object,
    data=NULL,
    scale_transform = "log1p"){

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
#' @param descriptor_variable either the name of a field in data containing the descriptor or a string to be used as the
#' plot title. If there are too more than 3 elements in the descriptor field, then `descriptor_variable` is used as the plot
#' title.
#'
#' @return a ggplot2 plot object with title, subtitle and adjusted theme.
#'
#' @family plotting_functions
#' @export
iidda_plot_settings <- function(plot_object,
                               data,
                               min_time = "min_time",
                               max_time = "max_time",
                               descriptor_name ="descriptor_name",
                               theme = theme_bw()){

  # if there are too many descriptors or the descriptor name doesn't exist in data,
  # use the provided descriptor string to label the plot
  if( !is.null(data[[descriptor_name]])){
    if( nrow(data[[descriptor_name]])<=3){
      descriptor_name <- data[[descriptor_name]] %>% pull
    } else {
      descriptor_name <- colnames(data[[descriptor_name]])
    }
  }
  plot_object+ggtitle(label=descriptor_name,subtitle=paste0(data[[min_time]]," to ",data[[max_time]])) + theme
}



#' Validate time variables
#'
#' Validate if variable is a date data type in the data set.
#'
#' @param var_nm string of variable name
#' @param data data frame
#'
#' @return boolean of validation status
#' @export
valid_time_vars = function(var_nm, data) {
  (var_nm %in% names(data)) & is.Date(data[[var_nm]])
}

#' Time units
#'
#' Vector of all possible time units, most or all are derived from lubridate functions
time_units = c(

  # these are single-argument functions from lubridate
  "mday", "qday", "yday",
  "week", "epiweek", "isoweek",
  "year",

  # these are multi-argument functions from lubridate that we are normalizing
  # to have single arguments
  "wday_num", "wday_ordered", "wday_factor",
  "wday_ordered_abbr", "wday_factor_abbr",
  "month_num", "month_ordered", "month_factor",
  "month_ordered_abbr", "month_factor_abbr" ,

  # these handle the month when the quarter starts
  "quarter_1", "quarter_2", "quarter_3", "quarter_4", "quarter_5",
  "quarter_6", "quarter_7", "quarter_8", "quarter_9", "quarter_10",
  "quarter_11", "quarter_12"
)

#' Lubridate functions
#'
#' lubridate functions with desired interpretable labels
lubridate_funcs = c(
  wday = 'Day of Week',
  mday = 'Day of Month',
  qday = 'Day of Quarter',
  yday = 'Day of Year',
  week = 'Week of Year',
  epiweek = 'Epi Week',
  isoweek = 'ISO Week',
  month = 'Month',
  quarter = 'Quarter',
  year = 'Year'
)

#' Get time unit labels
#'
#' Get label of associated time unit
#'
#' @param unit time unit, one of time_units
#'
#' @return label of associated time unit
#' @export
get_unit_labels = function(unit) {
  lubridate_funcs[index_sep(unit, 1L)]
}

#' Mutate time variables
#'
#' Create new time unit fields
#'
#' @param data data set containing an input time field
#' @param unit time unit, one of time_units
#' @param input_name field name in `data` containing input time field
#' @param output_name field name of newly created time unit field, by default uses get_unit_labels().
#'
#' @return all fields in `data` with additional time unit field
#' @export
mutate_time_vars = function(
  data,
  unit = unname(time_units),
  input_nm = "period_start_date",
  output_nm = get_unit_labels(unit)
) {
  #browser()
  stopifnot(valid_time_vars(input_nm, data))
  time_unit_func = make_time_trans(unit)
  mutate(data, !!output_nm := time_unit_func(get(input_nm)))
}

#' Get time transformation
#'
#' Get associated lubridate function to compute time unit.
#'
#' @param unit time unit, one or more of time_units
#'
#' @importFrom lubridate wday mday qday yday week epiweek isoweek month quarter year
#' @return function to compute time unit
#' @export
make_time_trans = function(unit = unname(time_units)) {

  unit = match.arg(unit)
  as_func = force
  lubridate_func_nm = index_sep(unit, 1L)
  lubridate_func = getFromNamespace(lubridate_func_nm, "lubridate")
  if (lubridate_func_nm %in% c("month", "wday")) {
    type = index_sep(unit, 2L)
    abbr = index_sep(unit, 3L) == "abbr"
    label = type != "num"
    if (type == "factor") as_func = getFromNamespace("factor", "base")
    unit_func = function(x) lubridate_func(x, label = label, abbr = abbr)
  } else if (lubridate_func_nm == "quarter") {
    fiscal_start = as.integer(index_sep(unit, 2L))
    unit_func = function(x) {
      lubridate_func(x, type = "quarter", fiscal_start = fiscal_start)
    }
  } else {
    unit_func = lubridate_func
  }

  function(x) as_func(unit_func(x))
}
