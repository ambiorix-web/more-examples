box::use(
  .. / ui / home[UI],
)

#' Handle GET at '/'
#'
#' @export
home_get <- function(req, res) {
  res$send(UI())
}
