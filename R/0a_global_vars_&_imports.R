# Global Variables --------------------------------------------------------

  if(getRversion() >= "2.15.1") utils::globalVariables(c(
    "."
  ))

  .units <- units::valid_udunits(TRUE)$symbol

# Imports -----------------------------------------------------------------

  #' @import magrittr
  NULL
