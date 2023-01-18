testthat::test_that("diseases without sub-classes can optionally pass blank strings", {
  testthat::expect_equal(
    ops$filter("Communicable Disease Incidence"
      ,disease = "Mumps"
      ,disease_subclass = ""
    ),
    ops$filter("Communicable Disease Incidence"
      ,disease = "Mumps"
    )
  )
})
