box::use(
  htmltools[
    tags,
    HTML,
    tagList,
  ],
  .. /
    utils /
    create_href[
      create_href,
    ],
  . / components / navbar[Navbar],
)

#' Generic UI page
#'
#' A generic bootstrap UI page.
#'
#' @param ... [htmltools::tags] /// Optional. Passed to the HTML
#'        document body.
#' @param title String /// Optional. Browser title.
#'        Defaults to "Tiny API".
#' @param page String /// Optional. The current page.
#'        Valid values are:
#'        - "home" (default)
#'        - "docs"
#'
#' @return [htmltools::tagList]
#'
#' @export
Page <- function(
  ...,
  title = "Tiny API",
  page = c("home", "docs")
) {
  page <- match.arg(arg = page)
  selected <- switch(
    EXPR = page,
    home = "/",
    docs = "/docs"
  )

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
          href = create_href(href = "/static/bootstrap-5.3.8/bootstrap.min.css")
        )
      ),
      tags$body(
        class = "bg-light",
        Navbar(selected = selected),
        ...,
        tags$script(
          src = create_href(
            href = "/static/bootstrap-5.3.8/bootstrap.bundle.min.js"
          )
        )
      )
    )
  )
}
