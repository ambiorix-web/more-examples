box::use(
  htmltools[
    tags,
    tagList,
  ],
  . /
    page[
      Page,
    ],
)

#' Home page UI
#'
#' @export
UI <- function() {
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

  Page(content)
}
