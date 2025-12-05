box::use(
  htmltools[
    tags,
    tagList,
  ],
)

#' File Input
#'
#' @param id String /// Required. Input ID.
#' @param label [htmltools::tags] /// Required. Input label.
#' @param ... [key=value] attributes /// Optional. Input tag attributes.
#'
#' @return [htmltools::tags]
#' @export
FileInput <- function(
  id,
  label,
  ...
) {
  tags$div(
    tags$label(
      `for` = id,
      class = "form-label",
      label
    ),
    tags$input(
      id = id,
      name = id,
      class = "form-control",
      type = "file",
      ...
    )
  )
}
