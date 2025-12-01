box::use(
  htmltools[
    tags,
    tagList,
  ],
)

#' Button
#'
#' @param id String /// Optional. Button ID.
#' @param label [htmltools::tags] /// Required. Button label.
#' @param class Character vector /// Optional. Classes to
#' apply to the button.
#'
#' @return [htmltools::tags]
#' @export
Button <- function(
  id = NULL,
  label,
  class = NULL,
  type = c("button", "submit", "reset")
) {
  tags$button(
    type = type,
    id = id,
    class = c("btn", class),
    label
  )
}
