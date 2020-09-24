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
