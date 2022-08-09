weight_status_who_youth <- function(
  bmi, sex = c("Male", "Female"),
  age_yrs = NULL, age_mos = NULL,
  severe_35 = TRUE
) {

  ## Disclaimer

    warning(
      "WHO BMI analysis is experimental and needs testing",
      call. = FALSE
    )

  ## Format variables

    sex %<>% sapply(percentile_sex, USE.NAMES = FALSE)

    reference <- lapply(
      sex, percentile_reference,
      standards = standards_who, check_age = FALSE
    )

    age_mos %<>% sapply(
      age_yrs, percentile_age,
      age_mos = ., min_age = 0, max_age = 228,
      USE.NAMES = FALSE
    )

    bmi %<>% units::drop_units(.)

  ## Retrieve element-wise data

    mapply(
      age_mos = age_mos,
      reference = reference,
      bmi = bmi,
      SIMPLIFY = FALSE,
      FUN = function(age_mos, reference, bmi) {

        #Prepare for calculations

        lesser_index <- percentile_index(reference, age_mos)

        greater_index <- lesser_index + 1

        increment <- age_mos - reference$Age[lesser_index]

        #Get Z score and percentile

        info <- mapply(
          percentile_lms,
          colname = c("L", "M", "S"),
          MoreArgs = list(
            reference = reference,
            lesser_index = lesser_index,
            greater_index = greater_index,
            increment = increment
          ),
          SIMPLIFY = FALSE
        )

        z_score <-
          c(info, bmi = list(bmi)) %>%
          do.call(percentile_z, .)

        percentile <-
          {stats::pnorm(z_score) * 1000} %>%
          floor(.) %>%
          {. / 10} %>%
          unname(.)

        bmi_severe <-
          do.call(percentile_back_calculate, info) %>%
          {. * 1.2}

        #Get classification

        classification <-
          cut(
            percentile,
            c(-Inf, 5, 85, 95, Inf),
            c("Underweight", "Healthy Weight", "Overweight", "Obese"),
            right = FALSE
          ) %>%
          as.character(.) %>%
          ifelse(
            (bmi >= bmi_severe) | (bmi >= 35 & severe_35),
            "Severe Obese",
            .
          ) %>%
          factor(c(
            "Underweight", "Healthy Weight", "Overweight", "Obese", "Severe Obese"
          ))

        #Finish up

        dplyr::tibble(
          bmi = bmi,
          classification = classification,
          percentile = percentile,
          cutoff = bmi_severe
        )

      }
    ) %>%
    do.call(rbind, .) %>%
    dplyr::mutate(bmi = as_bmi(bmi))

}
