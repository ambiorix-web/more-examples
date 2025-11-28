box::use(
  htmltools[
    tags,
    tagList,
  ],
)

#' Text Input
#'
#' @param ... Named attributes /// Optional. Passed to the input tag.
#' @param id String /// Required. Input ID.
#' @param label [htmltools::tags] /// Required. Input label.
#' @param type String /// Optional. Either "text" (default) or "email".
#'
#' @return [htmltools::tags]
#' @export
TextInput <- function(
  ...,
  id,
  label,
  type = c("text", "email")
) {
  type <- match.arg(arg = type)

  tags$div(
    tags$label(
      `for` = id,
      class = "form-label",
      label
    ),
    tags$input(
      id = id,
      name = id,
      type = type,
      class = "form-control",
      ...
    )
  )
}

#' Text Area Input
#'
#' @param ... Named attributes /// Optional. Passed to the textarea tag.
#' @param id String /// Required. Input ID.
#' @param label [htmltools::tags] /// Required. Input label.
#'
#' @return [htmltools::tags]
#' @export
TextAreaInput <- function(
  ...,
  id,
  label
) {
  tags$div(
    tags$label(
      class = "form-label",
      `for` = id,
      label
    ),
    tags$textarea(
      name = id,
      id = id,
      class = "form-control",
      rows = "3",
      ...
    )
  )
}

#' Radio Button Input
#'
#' @param id String /// Required. Input ID of the container.
#' @param choices [Named] List or character vector /// Required.
#'        Choices to show.
#'        If named, must be of the format `value = label`.
#' @param selected String /// Optional. The selected option in `choices`.
#' @param alignment String /// Optional. Alignment of the choices.
#'        Either "stacked" (default), or "inline".
#'
#' @return [htmltools::tagList]
#' @export
RadioButtonInput <- function(
  id,
  choices,
  selected = NULL,
  alignment = c("stacked", "inline")
) {
  alignment <- match.arg(arg = alignment)

  values <- names(choices)
  if (is.null(values)) {
    values <- choices
  }

  radios <- lapply(
    X = seq_along(choices),
    FUN = \(idx) {
      choice <- choices[[idx]]
      value <- values[[idx]]
      choice_id <- paste0(id, idx)

      checked <- NULL
      if (identical(choice, selected)) {
        checked <- NA
      }

      tags$div(
        class = c(
          "form-check",
          switch(
            EXPR = alignment,
            inline = "form-check-inline"
          )
        ),
        tags$input(
          class = "form-check-input",
          type = "radio",
          name = id,
          id = choice_id,
          checked = checked,
          value = value
        ),
        tags$label(
          class = "form-check-label",
          `for` = choice_id,
          choice
        )
      )
    }
  )

  tagList(radios)
}

#' Button
#'
#' @param label [htmltools::tags] /// Required. Button label.
#' @param class Character vector /// Optional. Classes to
#' apply to the button.
#' @param type String /// Optional. Button type. Either
#' "button" (default), "submit", or "reset".
#'
#' @return [htmltools::tags]
#' @export
Button <- function(
  ...,
  label,
  class = NULL,
  type = c("button", "submit", "reset")
) {
  type <- match.arg(arg = type)

  tags$button(
    type = type,
    class = c("btn", class),
    label,
    ...
  )
}

#' Success Toast
#'
#' @param message [htmltools::tags] Message to show.
#'
#' @return [htmltools::tags]
#' @export
AlertSuccess <- function(message) {
  tags$div(
    class = "alert alert-success",
    role = "alert",
    message
  )
}
