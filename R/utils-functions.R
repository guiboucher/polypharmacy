#' Utils
#'
#' Delete `NA`s and `NaN`s.
#'
#' @param x Vector.
#'
#' @return `x` without `NA`s or `NaN`s.
#' @keywords internal
#' @export
#' @examples
#' rmNA(c(1:5, NA))
#' rmNA(c(NaN, NA, 1))
#' rmNA(c(NaN, NA))
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
#' @keywords internal
#' @export
sunique <- function(x, decreasing = FALSE, na.last = FALSE) {
  return(sort(unique(x), decreasing = decreasing, na.last = na.last))
}
