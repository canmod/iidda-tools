test_that("guessing messages are given", {
  resolve_var_args = iidda.analysis:::resolve_var_args
  TestMethod = function() {
    function(data, period_mid_time_variable = NULL) {
      data = resolve_var_args(data, opt_variables = "period_mid_time_variable")
      return(data)
    }
  }
  test_method = TestMethod()
  .trash = (smallpox
    |> iidda_defaults_rm()
    |> test_method()
    |> expect_message("Guessed that period_mid_time")
  )
})
