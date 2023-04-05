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
