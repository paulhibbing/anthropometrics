percentile_age <- function(age_mos, age_yrs) {

  if (is.null(age_mos)) {

    if (is.null(age_yrs)) stop(
      "`age_mos` is NULL, but `age_yrs` has not been provided"
    )

    age_mos <-
      {age_yrs * 365.2425} %>% # Convert to age in days
      {. / 30.4375} # Then to age in months

  }

  if (age_mos < 23.5 | age_mos > 240.5) stop(
    "Age (in months) must fall in the interval [23.5, 240.5]"
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

  lesser_proportion <-
    reference[lesser_index, colname] * (1 - increment)

  greater_proportion <-
    reference[greater_index, colname] * increment

  lesser_proportion + greater_proportion

}

percentile_reference <- function(sex) {

  {standards_cdc$Sex == sex} %>%
  standards_cdc[., ] %T>%
  {stopifnot(
    !any(duplicated(.$Age)),
    all(diff(order(.$Age)) == 1),
    nrow(.) > 0
  )}

}

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
