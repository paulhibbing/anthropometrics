test_that("unit conversion works as expected", {

  initial <- "kg"
  eventual <- "lb"

  variable_passing <- unit_convert(65, initial, eventual)

  testthat::expect_equal(
    object = variable_passing, expected = 65*2.2,
    ignore_attr = TRUE, tolerance = 0.01
  )

  testthat::expect_s3_class(variable_passing, "units")

  testthat::expect_error(
    unit_convert(165, "St", m),
    "cannot convert St into m"
  )

  testthat::expect_equal(
    object = unit_convert(165, "cm", "in"), expected = 165/2.52,
    ignore_attr = TRUE, tolerance = 0.01
  )

  testthat::expect_error(
    get_bmi(175, 65, "St", "m"),
    "cannot convert St into kg"
  )

})
