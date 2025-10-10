## Standard words for things -- mostly names
## for variables used for particular purposes

#' Time units
#'
#' Vector of all possible time units, most or all are derived from lubridate functions
std_time_units = c(

  # these are single-argument functions from lubridate
  "mday", "qday", "yday",
  "week", "epiweek", "isoweek",
  "year",

  # these are multi-argument functions from lubridate that we are normalizing
  # to have single arguments
  "wday_num", "wday_ordered", "wday_factor",
  "wday_ordered_abbr", "wday_factor_abbr",
  "month_num", "month_ordered", "month_factor",
  "month_ordered_abbr", "month_factor_abbr",

  # these handle the month when the quarter starts
  "quarter_1", "quarter_2", "quarter_3", "quarter_4", "quarter_5",
  "quarter_6", "quarter_7", "quarter_8", "quarter_9", "quarter_10",
  "quarter_11", "quarter_12"
)
time_units = std_time_units ## back-compat

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

## reported incidence
std_cases_variables = function() {
  c(
      "cases_this_period"
    , "cases_prev_period"
    , "cases_this_period_mo"
    , "cases_this_period_qr"
    , "cases_this_period_sum"
    , "cases_this_period_wk"
    , "cases_this_period_yr"
    , "cases_prev_year"
    , "cases_two_years_ago"
    , "cases_cum_report_year"
    , "cases_cum_prev_year"
    , "cases_cum_median_prev_5_years"
    , "cases_median_prev_5_years"
  )
}

## demographics
std_population_variables = function() c("popultion", "population_reporting")
std_birth_variables = function() c("births", "christened")
std_death_variables = function() c("deaths", "burials")

#' Series variables
#'
#' List of column names that contain numerical information that could
#' represent a time series if processed appropriately.
std_series_variables = function() {
  c(
      std_death_variables()
    , std_cases_variables()
    , std_birth_variables()
    , std_population_variables()

    ## basically only for lbom
    , "infected_parishes"
    , "not_infected_parishes"
  )
}

std_median_cases_variables = function() {
  c(
      "cases_cum_median_prev_5_years"
    , "cases_median_prev_5_years"
  )
}

std_count_variables = function() {
  setdiff(
      std_series_variables()
    , std_median_cases_variables()
  )
}

std_total_count_variables = function() {
  sprintf("total_%s", std_count_variables())
}

std_norm_variables = function() {
  c(
      std_population_variables()
    , std_birth_variables()
    , std_death_variables()
  )
}


#' Time variables
#'
#' List of column names that contain information locating a point or
#' interval of time.
std_time_variables = function() {
  c(
      std_date_variables()
    , std_numeric_time_variables()
    , std_integer_time_variables()
  )
}

std_period_end_variables = function() "period_end_date"
std_period_mid_time_variables = function() "period_mid_time"
std_period_mid_date_variables = function() "period_mid_date"
std_period_mid_variables = function() {
  c(
      std_period_mid_date_variables()
    , std_period_mid_time_variables()
  )
}
std_period_start_variables = function() "period_start_date"

std_date_variables = function() {
  c(
      std_period_end_variables()
    , std_period_mid_variables()
    , std_period_start_variables()
    , "date"
  )
}

std_integer_time_variables = function() {
  c(
      "month"
    , "year"
    , "collection_year"
  )
}

std_numeric_time_variables = function() "numdate"

std_time_scale_variables = function() "time_scale"

std_period_width_variables = function() std_period_days_variables()

std_period_days_variables = function() "days_this_period"

## should probably include std_time_scale_variables as well?
std_time_group_variables = std_integer_time_variables

## variables that represent time in some way, but that are
## not required to be stored as date/time types
std_time_grouping_variables = function() {
  c(
      std_period_end_variables()
    , std_period_start_variables()
    , std_period_width_variables()
    , std_time_scale_variables()
  )
}

std_disease_variables = function() {
  c(
      "disease"
    , "nesting_disease"
    , "basal_disease"
  )
}


std_hierarchical_variables = function() {
  c(
      "disease"
    , "cause"
    , "location"
    , "sex"
    , "age_group"
  )
}
std_nesting_variables = function() {
  sprintf("nesting_%s", std_hierarchical_variables())
}
std_basal_variables = function() {
  sprintf("basal_%s", std_hierarchical_variables())
}

## variables, which if constant characters or factors, could
## be used as the title of a plot
std_title_variables = function() {
  c(
      "disease"
    , "cause"
    , "nesting_disease"
    , "nesting_cause"
    , "iso_3166"
    , "iso_3166_2"
    , "location"
    , "nesting_location"
    , "sex"
    , "nesting_sex"
    , "age_group"
    , "nesting_age_group"
    , "time_scale"
  )
}


std_grouping_variables = function() {
  x = c(
      std_hierarchical_variables()
    , std_nesting_variables()
    , std_basal_variables()
    , std_time_grouping_variables()
  )
  y = std_title_variables()
  c(x, y[!y %in% x])  ## no setdiff or union because order is important to get right
}

std_among_panel_variables = std_grouping_variables
std_within_panel_variables = std_grouping_variables
std_colour_variables = std_grouping_variables

std_category_variables = function() {
  x = c(
      std_hierarchical_variables()
    , std_nesting_variables()
    , std_basal_variables()
    , std_time_scale_variables()
  )
  y = std_title_variables()
  c(x, y[!y %in% x])  ## no setdiff or union because order is important to get right
}

# synonym
std_categorical_variables = std_category_variables


#' Data Prep Default Functions
#' 
#' @param data Data frame that likely comes from IIDDA.
#' @param cases_variable Name of variable containing unstandardized reported incidence.
#' @param population_variable Name of variable containing population numbers.
#' @param birth_variable Name of variable containing numbers of births.
#' @param death_variable Name of variable containing numbers of deaths.
#' @param series_variable Name of variable that can be used on the y-axis of a time-series.
#' @param median_cases_variable Name of variable containing the median of a set of count variables.
#' @param count_variable Name of variable containing a count (e.g., cases, births, deaths, population).
#' @param total_count_variable Name of variable containing a marginal total of a set of counts.
#' @param norm_variable Name of variable that can be used to normalize another variable (e.g., population normalized cases).
#' @param time_variable Name of variable that characterizes the temporal location of the time period.
#' @param period_end_variable Name of variable containing 
#' @param period_mid_time_variable Name of variable containing 
#' @param period_mid_date_variable Name of variable containing 
#' @param period_mid_variable Name of variable containing 
#' @param period_start_variable Name of variable containing 
#' @param date_variable Name of variable containing 
#' @param integer_time_variable Name of variable containing 
#' @param numeric_time_variable Name of variable containing 
#' @param time_scale_variable Name of variable containing 
#' @param period_width_variable Name of variable that characterizes the width of the time period.
#' @param period_days_variable Name of variable containing 
#' @param time_group_variable Name of variable containing 
#' @param time_grouping_variable Name of variable containing 
#' @param disease_variable Name of variable containing 
#' @param hierarchical_variable Name of variable containing 
#' @param nesting_variable Name of variable containing 
#' @param basal_variable Name of variable containing 
#' @param title_variable Name of variable containing 
#' @param grouping_variable Name of variable containing 
#' @param among_panel_variable Name of variable containing 
#' @param within_panel_variable Name of variable containing 
#' @param colour_variable Name of variable containing 
#' @param category_variable Name of variable containing 
#' @param categorical_variable Name of variable containing 
#' 
#' @name data_prep_default
NULL
