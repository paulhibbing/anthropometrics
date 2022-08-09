# Setup -------------------------------------------------------------------

  set.seed(610)

  df <- dplyr::tibble(

    bmi = pmax(pmin(rnorm(100, 25, 7), 80), 10),

    sex = sample(
      c("Female", "Male", "female", "male", "M", "F", "m", "f"),
      100,
      TRUE
    ),

    age_mos = pmin(pmax(rnorm(100, 132, 75), 23.5), 240.5)

  )


# Test --------------------------------------------------------------------

  test_that("CDC outputs remain equivalent with PAutilities", {

    pautilities <- PAutilities::get_BMI_percentile(
      weight_kg = NULL, height_cm = NULL, BMI = "bmi",
      sex = "sex", age_mos = "age_mos", df = df, output = "summary"
    )

    df$bmi <- as_bmi(df$bmi)

    current <- weight_status(
      df$bmi, "CDC youth",
      sex = df$sex, age_mos = df$age_mos
    )

    testthat::expect_equal(current, pautilities, ignore_attr = TRUE)

  })
