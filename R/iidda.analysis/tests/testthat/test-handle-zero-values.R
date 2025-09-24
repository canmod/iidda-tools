test_that("Zero handling works", {
  handler = HandleZeroValues()
  expect_identical(smallpox, handler(smallpox))

  handler = HandleZeroValues(zero_remove = TRUE)
  expect_equal(
      smallpox |> handler() |> nrow()
    , nrow(smallpox) - sum(smallpox$deaths == 0, na.rm = TRUE)
  )

  handler = HandleZeroValues(zero_replace = NA)
  expect_equal(sum(handler(smallpox)$deaths == 0, na.rm = TRUE), 0)

  expect_error(
      HandleZeroValues(zero_remove = TRUE, zero_replace = 0)
    , regexp = "Cannot both remove and replace zeros"
  )
})
