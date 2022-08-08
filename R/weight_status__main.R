#' Classify weight status
#'
#' @param bmi body mass index value(s). Must return \code{TRUE} when tested with
#'   \code{\link{is_bmi}}. See details
#' @param wt body mass value(s)
#' @param ht height value(s)
#' @inheritParams get_bmi
#' @param method character scalar. The desired classification method. Options
#'   are \code{"adult"} (the default), \code{"CDC youth"}, \code{"WHO youth"},
#'   and \code{"custom"}
#' @param select character. The desired output. Can be \code{"bmi"},
#'   \code{"percentile"}, \code{"classification"}, \code{"all"}, or a
#'   combination thereof. Percentiles will not be returned when \code{method} is
#'   \code{adult} or \code{custom}, and an error will be thrown if no other
#'   selections are made for those methods
#' @param ... arguments passed to the internal applicator that corresponds with \code{method}
#'
#' @details Two options are available for passing in values. The first is to
#'   supply a value for \code{bmi}, which must return \code{TRUE} when tested
#'   with \code{\link{is_bmi}}. If attempting to pass in BMI as a numeric value,
#'   it can be wrapped in \code{\link{as_bmi}} to ensure success. The second
#'   option is to pass weight and height in as separate values, which will be
#'   fed into \code{\link{get_bmi}} to determine the value for further
#'   classification. Regardless of how values are passed in, careful attention
#'   should be given to ensure units are correct. The code and internal checks
#'   have been set up to help with this as much as possible, but a good amount
#'   of onus is still on the user and can likely never be completely eliminated.
#'
#' @return A tibble indicating the BMI(s), percentile(s), and/or classification(s)
#'
#' @examples
#' weight_status(as_bmi(c(18, 20, 26, 31, 36, 42)))
#' weight_status(
#'   as_bmi(c(18, 20, 26, 31, 36, 42)),
#'   method = "custom", labels = NULL, right = TRUE
#' )
#'
#' @export
weight_status <- function(
  bmi, wt = NULL, ht = NULL,
  wt_units = "kg", ht_units = "cm",
  method = c("adult", "CDC youth", "WHO youth", "custom"),
  select = c("bmi", "percentile", "classification", "all"),
  ...
) {

  ## Validate input

    method <- match.arg(method)

    select %<>% weight_status_select(method)

  ## Ensure valid BMI/s to work with

    if (missing(bmi)) {
      if (is.null(wt) | is.null(ht)) stop(
        "'wt' and 'ht' must be specified if 'bmi' is not"
      )
      bmi <- get_bmi(wt, ht, wt_units, ht_units)
    } else {
      if (!is_bmi(bmi)) stop(
        "`bmi` must be passed as a valid BMI object:",
        " use `as_bmi()` to dodge this error"
      )
    }

  ## "Dispatch" to individual functions

    switch(
      method,
      "adult" = weight_status_adult(bmi, ...),
      "CDC youth" = weight_status_cdc_youth(bmi, ...),
      "WHO youth" = weight_status_who_youth(bmi, ...),
      "custom" = weight_status_custom(bmi, ...),
      stop(
        "Error linking `method` (", method, ")",
        " to a classification function"
      )
    ) %>%
    dplyr::select(dplyr::all_of(select))

}
