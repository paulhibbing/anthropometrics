#' Calculate energy storage
#'
#' @param fm fat mass
#' @param ffm fat-free mass
#' @param fm_units units of measure for \code{fm}. Can be quoted (recommended)
#'   or unquoted. Default is kg
#' @param ffm_units units of measure for \code{ffm}. Can be quoted (recommended)
#'   or unquoted. Defaults to the value passed for \code{fm_units} (helpful for
#'   piping)
#' @param factors character scalar. Conversion factors to use. Defaults to
#'   \code{"shook"}. Currently, no other factors are supported
#'
#' @return A numeric value for energy storage
#' @export
#'
#' @examples
#' get_es(20, 50, "kg")
get_es <- function(
    fm, ffm, fm_units = "kg", ffm_units = fm_units,
    factors = c("shook")
) {

  factors <- match.arg(factors)

  fm_units <-
    substitute(fm_units) %>%
    {if (is.name(.)) as.character(.) else fm_units}

  ffm_units <-
    if (substitute(ffm_units) == "fm_units") {
      fm_units
    } else {
      substitute(ffm_units) %>%
      {if (is.name(.)) as.character(.) else ffm_units}
    }

  fm %<>% unit_convert(fm_units, "kg")
  ffm %<>% unit_convert(ffm_units, "kg")

  as.numeric(
    .fm_factors[[factors]]*fm +
    .ffm_factors[[factors]]*ffm
  )

}
