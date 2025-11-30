#' Null coalescing operator
#'
#' @param x Value to check.
#' @param y Default value if x is NULL or empty string.
#'
#' @return x if not NULL/empty, otherwise y.
#' @export
`%||%` <- function(x, y) {
  if (is.null(x) || identical(x, "")) {
    return(y)
  }
  x
}
