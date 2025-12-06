#' Create File Path From Project Root Path
#'
#' @param ... Strings /// Optional. Paths. Appended
#'        to the project root path.
#'
#' @return String.
#'
#' @export
file_path <- function(...) {
  box::file(...)
}
