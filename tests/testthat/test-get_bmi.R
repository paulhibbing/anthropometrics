test_that("BMI operations work as expected", {

  testthat::expect_equal(
    object = get_bmi(75, 180), expected = 75/1.8^2,
    ignore_attr = TRUE, tolerance = 0.01
  )

  testthat::expect_s3_class(
    get_bmi(75:85, 180:190),
    "units"
  )

})
