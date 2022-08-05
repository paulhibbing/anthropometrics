# Global Variables --------------------------------------------------------

  if(getRversion() >= "2.15.1") utils::globalVariables(c(
    "."
  ))

  .units <- units::valid_udunits(TRUE)$symbol

  .fm_factors <- list(
    shook = 9500,
    default = NA_real_
  )

  .ffm_factors <- list(
    shook = 1020,
    default = NA_real_
  )

# Imports -----------------------------------------------------------------

  #' @import magrittr
  NULL
