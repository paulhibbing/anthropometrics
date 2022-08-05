#' Calculate body mass index
#'
#' @param wt bodyweight
#' @param ht height
#' @param wt_units units of measure for \code{wt}. Can be quoted (recommended)
#'   or unquoted. Default is kg
#' @param ht_units units of measure for \code{ht}. Can be quoted (recommended)
#'   or unquoted. Default is cm
#'
#' @return An S3 object with class \code{units}
#' @export
#'
#' @examples
#' get_bmi(75, 180)
get_bmi <- function(wt, ht, wt_units = "kg", ht_units = "cm") {

  unit_convert(wt, substitute(wt_units), "kg")/
  unit_convert(ht, substitute(ht_units), "m")^2

}

