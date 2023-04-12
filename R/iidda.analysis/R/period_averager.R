#' Obtain period midpoints and average daily rates for count data
#'
#' @param data Data frame with rows at minimum containing period start and end dates and a count variable.
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
#'                    , period_start_date = seq(as.Date("2023-04-03"), as.Date("2023-06-05"), by = 7)
#'                    , period_end_date = seq(as.Date("2023-04-09"), as.Date("2023-06-11"), by = 7)
#'                    , cases_this_period = sample(0:100, 10, replace = TRUE)
#'                    , location = "college"
#' )
#'
#' period_averager(data, keep_raw = TRUE, keep_cols = c("disease", "location"))
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
                              , count_col )}
  data %>%
  mutate(year = lubridate::year(.data[[start_col]])
         , num_days = as.numeric(round(difftime(.data[[end_col]], .data[[start_col]]))) + 1
         , period_mid_date = as.Date(.data[[start_col]]) + lubridate::seconds(num_days * as.numeric(lubridate::days(1))/2)
         , daily_rate = as.numeric(.data[[count_col]])/as.numeric(num_days)
  ) %>%
    select(any_of({{keep_cols}})

           , year
           , num_days
           , period_mid_date
           , daily_rate )
}
