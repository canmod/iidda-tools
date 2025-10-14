test_that("Trimming of leading and training works", {
  trimmer = TrimSeries()
  expect_equal(smallpox, trimmer(smallpox))

  trimmer = TrimSeries(zero_lead = TRUE, zero_trail = TRUE)

  expect_equal(
      nrow(smallpox) - nrow(trimmer(smallpox))
    , min(which(smallpox$deaths != 0)) - 1
  )
})
