box::use(
  .. /
    data /
    pages[
      pages,
      default_selection,
    ],
  .. /
    ui /
    render[
      UI,
    ],
  .. /
    ui /
    navigation[
      path_for,
    ],
)

#' Get default redirect path
#'
#' @return String path to first page's default panel/tab.
#' @export
default_path <- function() {
  default_page <- names(pages)[1]
  default_target <- default_selection(pages[[default_page]])
  path_for(default_page, default_target$panel, default_target$tab)
}

#' Handle GET at '/'
#'
#' @export
root_get <- function(req, res) {
  res$redirect(default_path(), status = 302L)
}

#' Handle GET at '/pages/'
#'
#' @export
pages_get <- function(req, res) {
  res$redirect(default_path(), status = 302L)
}

#' Handle GET at '/pages/:page'
#'
#' @export
page_get <- function(req, res) {
  page_id <- req$params$page

  defaults <- default_selection(pages[[page_id]])
  res$redirect(
    path_for(page_id, defaults$panel, defaults$tab),
    status = 302L
  )
}

#' Handle GET at '/pages/:page/:panel'
#'
#' @export
panel_get <- function(req, res) {
  page_id <- req$params$page
  panel_id <- req$params$panel

  page <- pages[[page_id]]
  default_tab <- names(page$panels[[panel_id]]$tabs)[1]

  res$redirect(
    path_for(page_id, panel_id, default_tab),
    status = 302L
  )
}

#' Handle GET at '/pages/:page/:panel/:tab'
#'
#' @export
tab_get <- function(req, res) {
  page_id <- req$params$page
  panel_id <- req$params$panel
  tab_id <- req$params$tab

  res$send(UI(req, page_id, panel_id, tab_id))
}
