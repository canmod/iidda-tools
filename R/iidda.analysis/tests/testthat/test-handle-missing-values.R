test_that("Missing value handling works", {
  handler = HandleMissingValues()

  expect_identical(
      smallpox
    , smallpox |> handler()
  )

  handler = HandleMissingValues(na_remove = TRUE)
  expect_equal(
      smallpox |> handler() |> nrow()
    , nrow(smallpox) - sum(is.na(smallpox$deaths))
  )

  handler = HandleMissingValues(na_replace = 0)
  expect_equal(sum(is.na(handler(smallpox)$deaths)), 0)

  expect_error(
      HandleMissingValues(na_remove = TRUE, na_replace = 0)
    , regexp = "Cannot both remove and replace missing values"
  )
})
