box::use(
  htmltools[
    tags,
    tagList,
  ],
)

#' Card
#'
#' @param ... [htmltools::tags] /// Optional. Elements passed
#' to the card body.
#' @param title String /// Required. Card title.
#' @param class Character vector /// Optional. Classes to apply
#' to the card div.
#' @param body_class Character vector /// Optional. Classes to
#' apply to the card body div.
#' @param title_class Character vector /// Optional. Classes to
#' apply to the card title.
#'
#' @return [htmltools::tags]
#' @export
Card <- function(
  ...,
  title,
  class = NULL,
  body_class = NULL,
  title_class = NULL
) {
  tags$div(
    class = c("card", class),
    tags$div(
      class = c("card-body", body_class),
      tags$h3(
        class = c("card-title", title_class),
        title
      ),
      ...
    )
  )
}
