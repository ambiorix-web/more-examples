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
)

#' Generic UI page
#'
#' A generic bootstrap UI page.
#'
#' @param ... [htmltools::tags] Passed to the HTML document body.
#' @param title String. Browser title.
#'
#' @return [htmltools::tagList]
#'
#' @export
Page <- function(..., title = "Deep Links") {
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
        ),
        tags$link(
          rel = "stylesheet",
          href = create_href(href = "/static/main.css")
        )
      ),
      tags$body(
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
