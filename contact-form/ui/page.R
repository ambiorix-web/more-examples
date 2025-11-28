box::use(
  htmltools[
    tags,
    HTML,
    tagList,
  ]
)

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
