# Main wrappers -----------------------------------------------------------

  weight_status_cdc_youth <- function(
    bmi, sex = c("Male", "Female"),
    age_yrs = NULL, age_mos = NULL,
    severe_35 = TRUE
  ) {


    weight_status_general_youth(
      bmi, sex, age_yrs, age_mos, severe_35,
      standards_cdc, TRUE, 23.5, 240.5, "CDC"
    )


  }


  weight_status_who_youth <- function(
      bmi, sex = c("Male", "Female"),
      age_yrs = NULL, age_mos = NULL,
      severe_35 = TRUE
  ) {

    warning(
      "`WHO youth` is experimental and needs testing",
      call. = FALSE
    )

    weight_status_general_youth(
      bmi, sex, age_yrs, age_mos, severe_35,
      standards_who, FALSE, 0, 228, "WHO"
    )


  }

# Common code -------------------------------------------------------------

  weight_status_general_youth <- function(
    bmi, sex, age_yrs, age_mos, severe_35,
    standards, check_age, min_age, max_age,
    method = c("CDC", "WHO")
  ) {

    ## Format variables

      method <- match.arg(method)

      sex %<>% sapply(percentile_sex, USE.NAMES = FALSE)

      reference <- lapply(
        sex, percentile_reference,
        standards = standards, check_age = check_age
      )

      if (!is.null(age_mos)) {

        age_mos %<>% sapply(
          percentile_age, age_yrs = age_yrs,
          min_age = min_age, max_age = max_age,
          USE.NAMES = FALSE
        )

      } else {

        if (is.null(age_yrs)) stop(
          "`age_mos` is NULL, but `age_yrs` has not been provided"
        )

        age_mos %<>% percentile_age(
          age_yrs, min_age = min_age, max_age = max_age
        )

      }

      bmi %<>% units::drop_units(.)

    ## Retrieve element-wise data

      mapply(
        age_mos = age_mos,
        reference = reference,
        bmi = bmi,
        MoreArgs = list(method = method),
        SIMPLIFY = FALSE,
        FUN = function(age_mos, reference, bmi, method) {

          #Prepare for calculations

            if (method == "CDC") {


              increment <-
                floor(age_mos + 0.5) %>%
                {age_mos - . + 0.5}

              greater_index <- percentile_index(reference, age_mos + 1)

              lesser_index <- percentile_index(reference, age_mos)


            } else if (method == "WHO") {


              lesser_index <- percentile_index(reference, age_mos)

              greater_index <- lesser_index + 1

              increment <- age_mos - reference$Age[lesser_index]


            } else {


              stop(
                "Error specifying method for `weight_status_general_youth`",
                call. = FALSE
              )


            }

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
