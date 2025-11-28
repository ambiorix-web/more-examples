library(ambiorix)
library(htmltools)

app <- Ambiorix$new()

app$static(path = "public", uri = "static")

#' Is app running in prod?
#'
#' @return Logical.
in_prod <- function() {
  identical(
    Sys.getenv("APP_ENV"),
    "prod"
  )
}

#' Create an anchor tag's href attribute
#'
#' @description
#' Generates the `href` for an anchor (`<a>`) tag. If the application is
#' running in a production environment, the given `path` is prefixed to
#' the `href` to ensure the correct base URL is used.
#'
#' @param href String /// Required. `href` attribute of an anchor tag (e.g., "/about").
#' @param base_path String /// Optional. Base path on which the app is deployed. eg.,
#' if the app is deployed at `https://try.ambiorix.dev/infinite-scroll`,
#' the environment variable `APP_BASE_PATH` should be set to `/infinite-scroll`.
#' The default value is obtained from the `APP_BASE_PATH` environment variable,
#' or it can be passed directly.
#'
#' @return String. The complete `href` for the anchor tag.
#'
#' @examples
#' # In production, this may return "/infinite-scroll/about":
#' create_href("/about")
#'
#' @export
create_href <- function(
  href,
  base_path = Sys.getenv("APP_BASE_PATH")
) {
  if (in_prod()) {
    href <- paste0(base_path, href)
  }

  href
}

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
          href = create_href(href = "/static/styles.css")
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
  img_src <- paste0(
    create_href(href = "/static/images/"),
    img_names
  )
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
