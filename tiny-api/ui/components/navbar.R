box::use(
  htmltools[
    tags,
    tagList,
  ],
  .. / .. / utils / create_href[create_href],
)

#' Navbar
#'
#' Bootstrap navbar component.
#'
#' @param brand String /// Optional. Brand text. Defaults to "Tiny API".
#' @param links List /// Optional. List of link lists with `href` and `label`.
#' @param selected String /// Optional. Selected nav link. Must be one of the
#'        `href` values in `links`.
#'
#' @return [htmltools::tags$nav]
#'
#' @export
Navbar <- function(
  brand = "Tiny API",
  links = list(
    list(href = "/", label = "Home"),
    list(href = "/docs", label = "Docs")
  ),
  selected = NULL
) {
  tags$nav(
    class = "navbar navbar-expand-lg bg-body-tertiary shadow-sm",
    tags$div(
      class = "container",
      tags$a(
        class = "navbar-brand",
        href = create_href(href = "/"),
        brand
      ),
      tags$button(
        class = "navbar-toggler",
        type = "button",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = "#navbarNav",
        `aria-controls` = "navbarNav",
        `aria-expanded` = "false",
        `aria-label` = "Toggle navigation",
        tags$span(class = "navbar-toggler-icon")
      ),
      tags$div(
        class = "collapse navbar-collapse justify-content-end",
        id = "navbarNav",
        tags$ul(
          class = "navbar-nav",
          lapply(links, function(link) {
            is_active <- identical(link$href, selected)
            link_class <- c("nav-link", if (is_active) "active")

            tags$li(
              class = "nav-item",
              tags$a(
                class = link_class,
                href = create_href(href = link$href),
                `aria-current` = if (is_active) "page",
                link$label
              )
            )
          })
        )
      )
    )
  )
}
