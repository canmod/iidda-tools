if (interactive()) source("tests/testthat/setup.R")
test_that("exercise currently feable harmonizer", {
  harmonizer = SeriesHarmonizer()
  expect_identical(
      smallpox |> select(period_end_date, deaths) |> arrange(period_end_date)
    , harmonizer(smallpox)
  )
  expect_identical(
      meningitis_on |> harmonizer() |> nrow()
    , 3083 |> as.integer()
  )
})
