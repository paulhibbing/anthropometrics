percentile_sex <- function(sex = c("error", "male", "female")) {

  sex <- tolower(sex)

  sex <- match.arg(sex)

  switch(
    sex,
    "male" = "M",
    "female" = "F",
    stop(
      "Could not match sex to the available",
      " options (\"Male\" or \"Female\")"
    )
  )

}


percentile_reference <- function(sex, standards, check_age) {

  {standards$Sex == sex} %>%
  standards[., ] %T>%
  {stopifnot(
    !any(duplicated(.$Age)),
    all(diff(order(.$Age)) == 1) | !check_age,
    nrow(.) > 0
  )}

}


percentile_age <- function(age_mos, age_yrs, min_age, max_age) {

  if (is.null(age_mos)) {

    age_mos <-
      {age_yrs * 365.2425} %>% # Convert to age in days
      {. / 30.4375} # Then to age in months

  }

  if (any(age_mos < min_age | age_mos > max_age)) stop(
    "Age (in months) must fall in the interval ",
    "[", min_age, ", ", max_age, "]", call. = FALSE
  )

  age_mos

}


percentile_index <- function(reference, age_mos) {

  {reference$Age <= age_mos} %>%
  which(.) %>%
  max(.)

}


percentile_lms <- function(
  reference, colname, lesser_index, greater_index, increment
) {

  if (greater_index == lesser_index) return(
    reference[lesser_index, colname]
  )

  interval <- diff(reference$Age[c(lesser_index, greater_index)])

  lesser_proportion <-
    reference[lesser_index, colname] * (1 - increment/interval)

  greater_proportion <-
    reference[greater_index, colname] * (increment/interval)

  lesser_proportion + greater_proportion

}

percentile_z <- function(bmi, L, M, S) {

  {bmi/M} %>%
  {.^L} %>%
  {.-1} %>%
    {./(L*S)}

}

percentile_back_calculate <- function(p = 0.95, L, M, S) {

  p %T>%
  {stopifnot(. >= 0, . <= 1)} %>%
  stats::qnorm(.) %>%
  {. * L * S} %>%
  {. * (M ^ L)} %>%
  {. + (M ^ L)} %>%
  {. ^ (1/L)}

}
