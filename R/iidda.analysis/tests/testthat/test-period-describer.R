if (interactive()) source("tests/testthat/setup.R")
describer = PeriodDescriber(mid_types = c("time", "date"))
expect_error(
    describer(select(smallpox, -period_start_date))
  , regex = "Data needs to"
)
describer(smallpox)
