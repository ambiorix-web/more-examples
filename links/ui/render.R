box::use(
  htmltools[
    tags,
    tagList,
  ],
  . /
    page[
      Page,
    ],
  . /
    navigation[
      PageNavbar,
      PanelNav,
      TabNav,
      Breadcrumb,
      TabContent,
      ShareCard,
    ],
  .. /
    data /
    pages[
      pages,
      default_selection,
    ],
  .. /
    data /
    random[
      generate_random_demo,
    ],
)

#' Render full page UI
#'
#' @param req Request object.
#' @param page_id Page ID.
#' @param panel_id Panel ID.
#' @param tab_id Tab ID.
#'
#' @return [htmltools::tagList]
#' @export
UI <- function(req, page_id, panel_id, tab_id) {
  page <- pages[[page_id]]
  panel <- page$panels[[panel_id]]
  tab <- panel$tabs[[tab_id]]

  params <- req$query

  context <- list(
    req = req,
    page_id = page_id,
    panel_id = panel_id,
    tab_id = tab_id,
    params = params
  )

  query_for_nav <- NULL

  if (identical(page_id, "simulations") && identical(panel_id, "random")) {
    context$random <- generate_random_demo(params)
    query_for_nav <- context$random$query
  }

  navbar <- PageNavbar(pages, page_id, default_selection)
  panel_nav <- PanelNav(page, page_id, panel_id, query_for_nav)
  tab_nav <- TabNav(panel, page_id, panel_id, tab_id, query_for_nav)
  breadcrumb <- Breadcrumb(
    page,
    panel,
    tab,
    page_id,
    panel_id,
    default_selection
  )
  tab_content <- TabContent(panel, tab, req, context)
  share_card <- ShareCard(req, page_id, panel_id, tab_id, query_for_nav)

  content <- tagList(
    navbar,
    tags$main(
      class = "py-5",
      tags$div(
        class = "container-xl d-flex flex-column gap-4",
        tags$section(
          class = "d-flex flex-column gap-2",
          tags$span(
            class = "badge badge-soft rounded-pill fw-semibold align-self-start",
            "PositConf 2025"
          ),
          tags$h1(class = "display-5 fw-semibold", page$title),
          tags$p(class = "lead text-secondary mb-0", page$tagline)
        ),
        breadcrumb,
        tags$div(
          class = "card border-0",
          tags$div(
            class = "card-body d-flex flex-column gap-3",
            panel_nav,
            tab_nav
          )
        ),
        tab_content,
        tags$div(class = "mt-2", share_card)
      )
    )
  )

  Page(
    content,
    title = sprintf("%s - Deep Links", page$title)
  )
}
