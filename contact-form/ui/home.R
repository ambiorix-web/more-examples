box::use(
  htmltools[
    tags,
    tagList,
  ],
  . /
    page[
      Page,
    ],

  .. /
    utils /
    create_href[
      create_href,
    ],
)

#' Home page UI
#'
#' @export
UI <- function() {
  content <- tags$div(
    class = "container",
    tags$div(
      class = "card my-5 border-0 shadow",
      tags$div(
        class = "card-body",
        tags$h3(
          class = "card-title",
          "Hello, World!"
        ),
        tags$p(
          "We're building a contact page form. Visit",
          tags$a(
            href = create_href(href = "/contact"),
            "/contact"
          ),
          "to see it."
        )
      )
    )
  )

  Page(content)
}
