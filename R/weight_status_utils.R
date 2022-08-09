weight_status_select <- function(select, method) {

  ## Establish initial values

    select <- match.arg(
      select,
      c("bmi", "percentile", "classification", "cutoff", "all"),
      TRUE
    )

    if ("all" %in% select) {
      select <- c("bmi", "percentile", "classification", "cutoff")
    }

  ## Determine if percentile needs to be removed,
  ## and whether that creates problems

    if (method %in% c("adult", "custom")) {

      select %<>%
        setdiff(c("percentile", "cutoff")) %T>%
        {if (length(.) == 0) stop(
          paste(sQuote(select), collapse = " and "),
          " is/are not valid for `select` when `method`",
          " is set to 'adult' or 'custom'", call. = FALSE
        )}

    }

  ## Make sure the ordering is consistent

    c("bmi", "percentile", "classification", "cutoff") %>%
    {.[. %in% select]}

}
