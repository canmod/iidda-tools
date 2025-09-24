library(iidda.analysis)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

d = data.frame(
  period_start_date = c("2000-01-01", "2000-02-05", "2000-03-25", "2000-03-01", "2000-12-12", "2000-12-18", "2000-12-21") |> as.Date(),
  period_end_date   = c("2000-01-08", "2000-03-05", "2000-04-25", "2000-06-01", "2000-12-17", "2000-12-20", "2000-12-22") |> as.Date()
)
d$cases_this_period = seq(0, length = nrow(d), by = 37)
period_averager(d)
#undebug(period_averager)
