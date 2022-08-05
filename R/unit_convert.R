#' Flexibly convert anthropometric values between units
#'
#' @param x a value or vector to convert
#' @param from current units. Can be quoted (recommended) or unquoted
#' @param to desired units. Can be quoted (recommended) or unquoted
#'
#' @return an S3 object with class \code{units}
#' @seealso \link[units]{set_units}
#' @export
#'
#' @examples
#' unit_convert(1, "in", "cm")
unit_convert <- function(x, from, to) {

  from <- substitute(from)
  to <- substitute(to)

  if (is.call(from)) {
    from <- as.character(
      eval(from, parent.frame())
    )
  }

  if (is.call(to)) {
    to <- as.character(
      eval(to, parent.frame())
    )
  }

  if (is.name(from)) {
    from <- as.character(from)
    is_unit <- from %in% .units
    if (!is_unit) {
      stopifnot(exists(from, envir = parent.frame()))
      from <- get(from, envir = parent.frame())
    }
  }

  if (is.name(to)) {
    to <- as.character(to)
    is_unit <- to %in% .units
    if (!is_unit) {
      stopifnot(exists(to, envir = parent.frame()))
      to <- get(to, envir = parent.frame())
    }
  }

  units::set_units(x, from, mode = "standard") %>%
  units::set_units(., to, mode = "standard")

}
