library(ambiorix)
library(htmltools)

app <- Ambiorix$new()

app$static(path = "public", uri = "/static")

#' Generic UI page
#'
#' A generic bootstrap UI page.
#'
#' @param ... [htmltools::tags] Passed to the HTML
#' document body.
#' @param title String. Browser title.
#'
#' @return [htmltools::tagList]
#'
#' @export
Page <- function(..., title = "Forms") {
  tagList(
    HTML("<!doctype html>"),
    tags$html(
      lang = "en",
      tags$head(
        tags$meta(charset = "utf-8"),
        tags$meta(
          name = "viewport",
          content = "width=device-width, initial-scale=1"
        ),
        tags$title(title),
        tags$link(
          rel = "stylesheet",
          href = "/static/bootstrap-5.3.8/bootstrap.min.css"
        ),
        tags$link(
          rel = "stylesheet",
          href = "/static/styles.css"
        )
      ),
      tags$body(
        ...,
        tags$script(src = "/static/bootstrap-5.3.8/bootstrap.bundle.min.js")
      )
    )
  )
}

#' Text Input
#'
#' @param id String /// Required. Input ID.
#' @param label [htmltools::tags] /// Required. Input label.
#' @param type String /// Optional. Either "text" (default) or "email".
#'
#' @return [htmltools::tags]
#' @export
TextInput <- function(id, label, type = c("text", "email")) {
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
      class = "form-control"
    )
  )
}

#' Text Area Input
#'
#' @param id String /// Required. Input ID.
#' @param label [htmltools::tags] /// Required. Input label.
#'
#' @return [htmltools::tags]
#' @export
TextAreaInput <- function(id, label) {
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
      rows = "3"
    )
  )
}

#' Radio Button Input
#'
#' @param id String /// Required. Input ID of the container.
#' @param choices Character vector /// Required. Choices to show.
#' @param selected String /// Optional. The selected option in `choices`.
#' @param alignment String /// Optional. Alignment of the choices.
#' Either "stacked" (default), or "inline".
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

  radios <- lapply(
    X = seq_along(choices),
    FUN = \(idx) {
      choice <- choices[[idx]]
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
          value = choice
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

home_get <- function(req, res) {
  content <- tags$div(
    class = "container",
    tags$h3("Hello, World!"),
    tags$p(
      "We're building a contact page. Visit",
      tags$a(
        href = "/contact",
        "/contact"
      ),
      "to see it."
    )
  )

  html <- Page(content)

  res$send(html)
}

#' Handle GET at '/contact'
#'
#' @export
contact_get <- function(req, res) {
  content <- tags$div(
    class = "container",
    tags$div(
      class = "card my-5 border-0 shadow",
      tags$div(
        class = "card-body p-5",
        tags$h3(
          class = "card-title mb-3",
          "Contact Us"
        ),
        tags$form(
          action = "/contact",
          method = "post",
          tags$div(
            class = "row mb-3",
            tags$div(
              class = "col-12 col-md-6",
              TextInput(id = "full_name", label = "Full Name")
            ),
            tags$div(
              class = "col-12 col-md-6",
              TextInput(id = "email", label = "Email", type = "email")
            )
          ),
          tags$div(
            class = "mb-3",
            tags$p(
              class = "mb-1",
              "Type of Inquiry"
            ),
            RadioButtonInput(
              id = "inquiry_type",
              choices = c("General Inquiry", "Price Inquiry"),
              selected = "General Inquiry",
              alignment = "inline"
            )
          ),
          tags$div(
            class = "mb-3",
            TextAreaInput(id = "message", label = "Message")
          ),
          tags$div(
            class = "d-grid",
            Button(label = "Submit", type = "submit", class = "btn-primary")
          )
        )
      )
    )
  )

  html <- Page(content)

  res$send(html)
}

#' Handle POST at '/contact'
#'
#' @export
contact_post <- function(req, res) {
  data <- parse_form_urlencoded(req)
  print(data)
  # perform action to `data`: save to db, send email, etc.

  content <- tags$div(
    class = "container",
    tags$div(
      class = "my-5",
      AlertSuccess(
        message = tags$p(
          class = "mb-0",
          "Thank you! Your request has been received. Our team will get back to you within 48 hours.",
          tags$a(
            href = "/contact",
            "Submit another request?"
          )
        )
      )
    )
  )
  html <- Page(content)

  res$send(html)
}

app$get("/", home_get)
app$get("/contact", contact_get)
app$post("/contact", contact_post)

app$start(port = 8000L)
