library(iidda.analysis); library(testthat)
smallpox = ("smallpox.rds"
  |> system.file(package = "iidda.analysis")
  |> readRDS()
  |> iidda_defaults(
      series_variable = "deaths"
    , time_variable = "period_end_date"
    , period_start_variable = "period_start_date"
    , period_end_variable = "period_end_date"
    , period_mid_date_variable = "period_mid_date"
    , period_mid_time_variable = "period_mid_time"
    , period_days_variable = "days_this_period"
  )
)
meningitis_on = ("meningitis_on.rds"
  |> system.file(package = "iidda.analysis")
  |> readRDS()
  |> iidda_defaults(
      series_variable = "cases_this_period"
    , time_variable = "period_end_date"
  )
)
measles_1800_to_1842 = ("measles_1800_to_1842.rds"
  |> system.file(package = "iidda.analysis")
  |> readRDS()
  |> iidda_defaults(
      series_variable = "deaths"
    , time_variable = "period_end_date"
  )
)


names(smallpox)
