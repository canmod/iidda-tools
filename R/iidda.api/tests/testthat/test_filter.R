testthat::test_that("diseases without sub-classes can optionally pass blank strings", {
  ops = options(iidda_api_all_char = TRUE, iidda_api_msgs = FALSE)
  testthat::expect_equal(
    ops_staging$filter("Communicable Disease Incidence"
      ,historical_disease = "Mumps"
      ,historical_disease_subclass = ""
    ),
    ops_staging$filter("Communicable Disease Incidence"
      ,historical_disease = "Mumps"
    )
  )
  options(ops)
})
