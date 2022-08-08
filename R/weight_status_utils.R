weight_status_select <- function(select, method) {

  ## Establish initial values

    select <- match.arg(
      select,
      c("bmi", "percentile", "classification", "all"),
      TRUE
    )

    if ("all" %in% select) {
      select <- c("bmi", "percentile", "classification")
    }

  ## Determine if percentile needs to be removed,
  ## and whether that creates problems

    if (method %in% c("adult", "custom")) {

      select %<>%
        setdiff("percentile") %T>%
        {if (length(.) == 0) stop(
          "`percentile` is not a valid `select` setting",
          " when `method` is `adult` or `custom`", call. = FALSE
        )}

    }

  ## Make sure the ordering is consistent

    c("bmi", "percentile", "classification") %>%
    {.[. %in% select]}

}
