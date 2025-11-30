box::use(
  htmltools[
    tags,
    tagList,
  ],
)

#' Build URL path for page/panel/tab
#'
#' @param page Page ID.
#' @param panel Optional panel ID.
#' @param tab Optional tab ID.
#'
#' @return String URL path.
#' @export
path_for <- function(page, panel = NULL, tab = NULL) {
  segments <- list(page, panel, tab)
  segments <- Filter(
    f = function(val) {
      !is.null(val) && !identical(val, "")
    },
    segments
  )

  encoded <- vapply(
    X = segments,
    FUN = function(value) {
      utils::URLencode(value, reserved = TRUE)
    },
    FUN.VALUE = character(1)
  )

  paste0("/pages/", paste(encoded, collapse = "/"))
}

#' Compose href with query parameters
#'
#' @param path URL path.
#' @param params Optional list of query parameters.
#'
#' @return String URL with query parameters.
#' @export
compose_href <- function(path, params = NULL) {
  if (is.null(params) || length(params) == 0) {
    return(path)
  }

  encoded <- vapply(
    X = names(params),
    FUN = function(name) {
      value <- params[[name]]
      if (is.null(value) || identical(value, "")) {
        return("")
      }
      paste0(name, "=", utils::URLencode(as.character(value), reserved = TRUE))
    },
    FUN.VALUE = character(1)
  )

  encoded <- encoded[nzchar(encoded)]

  if (!length(encoded)) {
    return(path)
  }

  paste0(path, "?", paste(encoded, collapse = "&"))
}

#' Build page navbar
#'
#' @param pages List of page definitions.
#' @param active_page Currently active page ID.
#' @param default_selection_fn Function to get default panel/tab for a page.
#'
#' @return [htmltools::tags]
#' @export
PageNavbar <- function(pages, active_page, default_selection_fn) {
  first_page_id <- names(pages)[1]
  first_defaults <- default_selection_fn(pages[[first_page_id]])

  items <- lapply(names(pages), function(page_id) {
    page <- pages[[page_id]]
    defaults <- default_selection_fn(page)
    active <- identical(page_id, active_page)

    link_classes <- c("nav-link", "fw-semibold")
    if (active) {
      link_classes <- c(link_classes, "active", "text-primary")
    } else {
      link_classes <- c(link_classes, "text-secondary")
    }

    tags$li(
      class = "nav-item",
      tags$a(
        class = paste(link_classes, collapse = " "),
        href = path_for(page_id, defaults$panel, defaults$tab),
        page$title
      )
    )
  })

  tags$nav(
    class = "navbar navbar-expand-lg bg-white shadow-sm",
    tags$div(
      class = "container-xl",
      tags$a(
        class = "navbar-brand fw-semibold",
        href = path_for(
          first_page_id,
          first_defaults$panel,
          first_defaults$tab
        ),
        "Deep Links"
      ),
      tags$button(
        class = "navbar-toggler",
        type = "button",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = "#page-nav",
        `aria-controls` = "page-nav",
        `aria-expanded` = "false",
        `aria-label` = "Toggle navigation",
        tags$span(class = "navbar-toggler-icon")
      ),
      tags$div(
        class = "collapse navbar-collapse",
        id = "page-nav",
        tags$ul(class = "navbar-nav ms-auto gap-lg-3", items)
      )
    )
  )
}

#' Build panel navigation tabs
#'
#' @param page Page definition.
#' @param page_id Page ID.
#' @param active_panel Currently active panel ID.
#' @param query Optional query parameters.
#'
#' @return [htmltools::tags]
#' @export
PanelNav <- function(page, page_id, active_panel, query = NULL) {
  items <- lapply(names(page$panels), function(panel_id) {
    panel <- page$panels[[panel_id]]
    defaults <- list(tab = names(panel$tabs)[1])
    active <- identical(panel_id, active_panel)

    link_classes <- c("nav-link")
    if (active) {
      link_classes <- c(link_classes, "active")
    }

    tags$li(
      class = "nav-item",
      tags$a(
        href = compose_href(path_for(page_id, panel_id, defaults$tab), query),
        class = paste(link_classes, collapse = " "),
        panel$title
      )
    )
  })

  tags$ul(class = "nav nav-tabs flex-wrap", items)
}

#' Build tab navigation pills
#'
#' @param panel Panel definition.
#' @param page_id Page ID.
#' @param panel_id Panel ID.
#' @param active_tab Currently active tab ID.
#' @param query Optional query parameters.
#'
#' @return [htmltools::tags]
#' @export
TabNav <- function(panel, page_id, panel_id, active_tab, query = NULL) {
  tabs <- lapply(names(panel$tabs), function(tab_id) {
    tab <- panel$tabs[[tab_id]]
    active <- identical(tab_id, active_tab)

    link_classes <- c("nav-link", "py-1", "px-3")
    if (active) {
      link_classes <- c(link_classes, "active")
    }

    tags$li(
      class = "nav-item",
      tags$a(
        href = compose_href(path_for(page_id, panel_id, tab_id), query),
        class = paste(link_classes, collapse = " "),
        tab$title
      )
    )
  })

  tags$ul(class = "nav nav-pills flex-wrap gap-2", tabs)
}

#' Build breadcrumb navigation
#'
#' @param page Page definition.
#' @param panel Panel definition.
#' @param tab Tab definition.
#' @param page_id Page ID.
#' @param panel_id Panel ID.
#' @param default_selection_fn Function to get default panel/tab for a page.
#'
#' @return [htmltools::tags]
#' @export
Breadcrumb <- function(
  page,
  panel,
  tab,
  page_id,
  panel_id,
  default_selection_fn
) {
  page_defaults <- default_selection_fn(page)
  panel_default_tab <- names(panel$tabs)[1]

  tags$nav(
    `aria-label` = "breadcrumb",
    tags$ol(
      class = "breadcrumb mb-3",
      tags$li(
        class = "breadcrumb-item",
        tags$a(
          href = path_for(page_id, page_defaults$panel, page_defaults$tab),
          page$title
        )
      ),
      tags$li(
        class = "breadcrumb-item",
        tags$a(
          href = path_for(page_id, panel_id, panel_default_tab),
          panel$title
        )
      ),
      tags$li(
        class = "breadcrumb-item active",
        `aria-current` = "page",
        tab$title
      )
    )
  )
}

#' Build tab content
#'
#' @param panel Panel definition.
#' @param tab Tab definition.
#' @param req Request object.
#' @param context Context object.
#'
#' @return [htmltools::tags]
#' @export
TabContent <- function(panel, tab, req, context) {
  intro <- panel$intro
  if (is.null(intro)) {
    intro <- ""
  }

  body <- tab$body
  if (is.function(body)) {
    body <- body(req, context)
  }

  tags$div(
    class = "card shadow-sm border-0",
    tags$div(
      class = "card-body",
      tags$h2(class = "h4 mb-3", tab$title),
      if (!identical(intro, "")) {
        tags$p(class = "text-muted", intro)
      },
      body
    )
  )
}

#' Build share card
#'
#' @param req Request object.
#' @param page_id Page ID.
#' @param panel_id Panel ID.
#' @param tab_id Tab ID.
#' @param query Optional query parameters.
#'
#' @return [htmltools::tags]
#' @export
ShareCard <- function(req, page_id, panel_id, tab_id, query = NULL) {
  scheme <- req$rook.url_scheme
  if (is.null(scheme)) {
    scheme <- "http"
  }

  host <- req$HTTP_HOST
  if (is.null(host)) {
    host <- req$SERVER_NAME
  }
  if (is.null(host)) {
    host <- "localhost"
  }

  relative <- compose_href(path_for(page_id, panel_id, tab_id), query)
  absolute <- paste0(scheme, "://", host, relative)

  tags$div(
    class = "card bg-dark text-white border-0",
    tags$div(
      class = "card-body",
      tags$h2(class = "h5 mb-3", "Share this exact view"),
      tags$p(
        class = "mb-3 text-light",
        "Copy the link below-anyone opening it lands directly in this nested tab."
      ),
      tags$div(
        class = "input-group mb-2",
        tags$span(class = "input-group-text", "URL"),
        tags$input(
          type = "text",
          class = "form-control",
          value = absolute,
          readonly = "readonly",
          `aria-label` = "Sharable link"
        )
      ),
      tags$p(
        class = "mb-0 text-light",
        "Route only: ",
        tags$code(class = "text-warning", relative)
      )
    )
  )
}
