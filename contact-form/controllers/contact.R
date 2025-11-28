box::use(
  ambiorix[
    parse_form_urlencoded,
  ],
  htmltools[
    tags,
    tagList,
  ],
  .. /
    ui /
    contact[
      UI,
    ],
  .. /
    ui /
    components[
      AlertSuccess
    ],
  .. /
    ui /
    page[
      Page,
    ],
)

#' Handle GET at '/contact'
#'
#' @export
contact_get <- function(req, res) {
  res$send(UI())
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
