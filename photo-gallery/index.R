library(ambiorix)
library(htmltools)

app <- Ambiorix$new()

app$static(path = "public", uri = "static")

#' Generic UI page
#'
#' @param ... [htmltools::tags] Passed to the HTML
#' document body.
#'
#' @return [htmltools::tagList]
#'
#' @export
Page <- function(...) {
  tagList(
    HTML("<!doctype html>"),
    tags$html(
      lang = "en",
      tags$head(
        tags$title("Photo Gallery"),
        tags$link(
          rel = "stylesheet",
          href = "/static/styles.css"
        )
      ),
      tags$body(
        ...
      )
    )
  )
}

#' Handle GET at '/'
#'
#' @export
home_get <- function(req, res) {
  img_names <- list.files(path = file.path("public", "images"))
  img_src <- paste0("/static/images/", img_names)
  img_tags <- lapply(
    X = img_src,
    FUN = \(src) {
      tags$img(
        src = src,
        loading = "lazy"
      )
    }
  )
  img_content <- tags$div(
    class = "img-content",
    img_tags
  )

  html <- Page(img_content)

  res$send(html)
}

app$get("/", home_get)

app$start(port = 8000L)
