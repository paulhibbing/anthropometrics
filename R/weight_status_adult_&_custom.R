#' @rdname weight_status
#' @keywords internal
weight_status_adult <- function(bmi) {

  weight_status_cut(
    bmi,
    cutoffs = c(-Inf, 18.5, 25, 30, 35, 40, Inf),
    labels = c(
      "Underweight", "Healthy Weight", "Overweight",
      "Class 1 Obese", "Class 2 Obese", "Class 3 Obese"
    ),
    FALSE
  )

}

#' @param cutoffs The BMI cutoffs to use when determining weight status for
#'   \code{weight_status_custom}. Default is the same cutoffs used for
#'   \code{weight_status_adult}
#' @param labels Character labels to assign for intervals based on
#'   \code{cutoffs}. Must be either \code{NULL} or else a character vector of
#'   length one less than \code{length(cutoffs)}. See ?cut for more information
#' @param right See ?cut
#' @param check_cutoffs logical. Should custom cutoffs be checked with warnings
#'   issued when potential issues are detected?
#'
#' @rdname weight_status
#' @keywords internal
weight_status_custom <- function(
  bmi, cutoffs = c(-Inf, 18.5, 25, 30, 35, 40, Inf),
  labels = c(
    "Underweight", "Healthy Weight", "Overweight",
    "Class 1 Obese", "Class 2 Obese", "Class 3 Obese"
  ),
  right = FALSE, check_cutoffs = TRUE
) {

  if (check_cutoffs & !is.infinite(cutoffs[1])) warning(
    "First element of `cutoffs` should probably be -Inf.",
    " Set `check_cutoffs = FALSE` to suppress this warning."
  )

  if (check_cutoffs & !is.infinite(cutoffs[length(cutoffs)])) warning(
    "Last element of `cutoffs` should probably be Inf.",
    " Set `check_cutoffs = FALSE` to suppress this warning."
  )

  if (!is.null(labels) & length(labels) != length(cutoffs) - 1) {
    stop(
      "`labels` must be NULL or else have length one less than",
      " the length of `cutoffs`. See ?cut"
    )
  }

  weight_status_cut(bmi, cutoffs, labels, right)

}

weight_status_cut <- function(bmi, cutoffs, labels, right) {

  dplyr::tibble(
   bmi = bmi,
   classification = cut(bmi, cutoffs, labels, right = right)
  )

}
