#' Validate BMI value(s)
#'
#' @param bmi numeric. BMI value(s) to check
#'
#' @examples
#' is_bmi(29)
#' as_bmi(29)
#' is_bmi(as_bmi(29))
#'
#' @name bmi-class
#' @export
is_bmi <- function(bmi) {

  bmi_check(bmi, FALSE) %>%
  {all(
    inherits(., "units"),
    attr(., "units")$numerator == "kg",
    identical(attr(., "units")$denominator, c("m", "m"))
  )}

}

#' @rdname bmi-class
#' @export
as_bmi <- function(bmi) {

  units::set_units(bmi, "kg/m^2") %>%
  bmi_check(.)

}

bmi_check <- function(bmi, check_format = TRUE) {

  if (inherits(bmi, "units"))
       bmi_num <- units::drop_units(bmi)
  else bmi_num <- as.numeric(bmi)

  low <- bmi_num < 10
  high <- bmi_num > 80

  if (any(low)) warning(
    sum(low), " BMI value(s) < 10 detected. Make sure you have",
    " specified BMI correctly (kg/m^2)", call. = FALSE
  )

  if (any(high)) warning(
    sum(high), " BMI value(s) > 80 detected. Make sure you have",
    " specified BMI correctly (kg/m^2)", call. = FALSE
  )

  ## Return the original BMI object (with any attributes/classes)
  bmi

}
