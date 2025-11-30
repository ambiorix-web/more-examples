box::use(
  htmltools[
    tags,
    tagList,
  ],
  .. /
    data /
    random[
      random_distribution_labels,
    ],
  . /
    navigation[
      path_for,
    ],
)

#' Metric Cards
#'
#' Display a row of metric cards with optional trend badges.
#'
#' @param metrics List of metric objects with label, value, and optional trend.
#'
#' @return [htmltools::tags]
#' @export
MetricCards <- function(metrics) {
  cards <- lapply(metrics, function(metric) {
    trend_badge <- NULL
    if (!is.null(metric$trend)) {
      badge_class <- if (metric$trend$status == "up") {
        "badge rounded-pill bg-success-subtle text-success"
      } else {
        "badge rounded-pill bg-danger-subtle text-danger"
      }
      trend_badge <- tags$span(
        class = badge_class,
        metric$trend$text
      )
    }

    tags$div(
      class = "col-md-4",
      tags$div(
        class = "card border-0 shadow-sm h-100",
        tags$div(
          class = "card-body",
          tags$p(class = "text-uppercase text-muted small mb-2", metric$label),
          tags$h3(class = "fw-semibold mb-2", metric$value),
          trend_badge
        )
      )
    )
  })

  tags$div(class = "row g-3", cards)
}

#' Bootstrap Table
#'
#' Generate a Bootstrap-styled HTML table from a data frame.
#'
#' @param df Data frame to display.
#' @param caption Optional table caption.
#' @param class CSS class(es) for the table.
#'
#' @return [htmltools::tags]
#' @export
BootstrapTable <- function(
  df,
  caption = NULL,
  class = "table table-sm table-striped align-middle mb-0"
) {
  header <- lapply(names(df), tags$th)
  rows <- lapply(seq_len(nrow(df)), function(i) {
    tags$tr(lapply(df[i, , drop = TRUE], function(value) {
      tags$td(as.character(value))
    }))
  })

  tags$table(
    class = class,
    if (!is.null(caption)) {
      tags$caption(class = "caption-top text-muted", caption)
    },
    tags$thead(tags$tr(header)),
    tags$tbody(rows)
  )
}

#' Random Distribution Control Form
#'
#' Form for selecting distribution type and sample size.
#'
#' @param page_id Page ID.
#' @param panel_id Panel ID.
#' @param tab_id Tab ID.
#' @param random_ctx Random demo context.
#'
#' @return [htmltools::tags]
#' @export
RandomControlForm <- function(page_id, panel_id, tab_id, random_ctx) {
  action <- path_for(page_id, panel_id, tab_id)
  selected_dist <- random_ctx$dist
  n <- random_ctx$n

  radio_inputs <- lapply(names(random_distribution_labels), function(code) {
    id <- sprintf("dist-%s", code)
    tags$div(
      class = "form-check",
      tags$input(
        class = "form-check-input",
        type = "radio",
        name = "dist",
        id = id,
        value = code,
        checked = if (identical(code, selected_dist)) "checked" else NULL
      ),
      tags$label(
        class = "form-check-label",
        `for` = id,
        random_distribution_labels[[code]]
      )
    )
  })

  tags$form(
    class = "row g-3 align-items-end",
    method = "get",
    action = action,
    tags$div(
      class = "col-md-6",
      tags$label(class = "form-label fw-semibold", "Distribution"),
      radio_inputs
    ),
    tags$div(
      class = "col-md-6",
      tags$label(class = "form-label fw-semibold", "Observations"),
      tags$input(
        type = "number",
        class = "form-control",
        name = "n",
        value = n,
        min = "1",
        max = "1000"
      ),
      tags$div(
        class = "form-text",
        "1 to 1000"
      ),
      tags$button(
        type = "submit",
        class = "btn btn-primary w-100",
        "Update view"
      )
    )
  )
}
