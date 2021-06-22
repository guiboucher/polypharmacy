#' Utils
#'
#' Delete `NA`s and `NaN`s.
#'
#' @param x Vector.
#'
#' @return `x` without `NA`s or `NaN`s.
#' @keywords internal
#' @encoding UTF-8
#' @export
#' @examples
#' x <- c(1:10, NA, NaN)
#' rmNA(x)
rmNA <- function(x) {
  if (anyNA(x)) {
    return(x[!is.na(x)])
  } else {
    return(x)
  }
}


#' Utils
#'
#' Combination of `sort()` and `unique()`.
#'
#' @param x Vector to sort and remove duplicates.
#' @param decreasing `TRUE` or `FALSE`.
#' @param na.last `NA` removes `NA`s, `TRUE` show `NA`s at the end and `FALSE` show `NA`s at the beginning.
#'
#' @return `x` sorted and without duplicates.
#' @keywords internal
#' @encoding UTF-8
#' @export
#' @examples
#' sunique(c(1, 1, 1, 2, 2, NA, NA))
#' sunique(c(1, 1, 1, 2, 2, NA, NA), na.last = TRUE)
#' sunique(c(1, 1, 1, 2, 2, NA, NA), na.last = NA)
#' sunique(c(1, 1, 1, 2, 2, NA, NA), decreasing = TRUE)
sunique <- function(x, decreasing = FALSE, na.last = FALSE) {
  return(sort(unique(x), decreasing = decreasing, na.last = na.last))
}


#' Statistic functions
#'
#' Determine the percentile from a *qX* value where *X* is a number from 1 to 3.
#'
#' @param x Character string, a quantile function, quarter values.
#'
#' @return Number {25, 50, 75}
#' @keywords internal
#' @encoding UTF-8
#' @export
#' @examples
#' stat_quantile_prob("q1")
#' stat_quantile_prob("q2")
#' stat_quantile_prob("q3")
stat_quantile_prob <- function(x) {
  ### If we want a quantile, we need to determine the probability
  if (stringr::str_detect(x, "q")) {
    if (x == "q1") {
      x <- "p25"
    } else if (x == "q2") {
      x <- "p50"
    } else if (x == "q3") {
      x <- "p75"
    } else {
      stop("ind_simult.stat_quantile_prob(): wrong value.")
    }
  }
  return(as.numeric(stringr::str_remove(x, "p")))
}
