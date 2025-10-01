library(ambiorix)
library(htmltools)
library(base64enc)

`%||%` <- function(x, y) {
  if (is.null(x) || identical(x, "")) {
    return(y)
  }
  x
}

format_number <- function(x, digits = 0L) {
  rounded <- round(x, digits = digits)

  format(
    rounded,
    big.mark = ",",
    trim = TRUE,
    scientific = FALSE,
    nsmall = digits
  )
}

format_percent <- function(x, digits = 1L) {
  sprintf(paste0("%.", digits, "f%%"), x * 100)
}

trend_info <- function(
  current,
  baseline,
  label,
  direction = c("up-is-good", "down-is-good")
) {
  direction <- match.arg(arg = direction)

  if (is.null(baseline) || is.na(baseline) || identical(baseline, 0)) {
    return(NULL)
  }

  change <- (current - baseline) / baseline
  status <- if (direction == "up-is-good") {
    if (change >= 0) "up" else "down"
  } else {
    if (change <= 0) "up" else "down"
  }

  list(
    text = sprintf("%+.1f%% %s", change * 100, label),
    status = status
  )
}

metric_cards <- function(metrics) {
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

bootstrap_table <- function(
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

random_distribution_labels <- c(
  norm = "Normal",
  unif = "Uniform",
  lnorm = "Log-normal",
  exp = "Exponential"
)

sanitize_dist <- function(dist) {
  if (is.null(dist) || !dist %in% names(random_distribution_labels)) {
    return("norm")
  }
  dist
}

sanitize_n <- function(n) {
  if (is.null(n)) {
    n <- 500L
  }

  n <- as.integer(n)

  if (is.na(n)) {
    n <- 500L
  }

  n <- max(1L, min(1000L, n))
  n
}

generate_histogram_src <- function(values, dist, n) {
  path <- tempfile(fileext = "png")
  on.exit(unlink(x = path))

  png(filename = path, width = 800, height = 500)
  hist(
    values,
    main = sprintf("r%s(%s)", dist, n),
    col = "#75AADB",
    border = "white"
  )
  dev.off()

  binary <- readBin(
    con = path,
    what = "raw",
    n = file.info(path)$size
  )

  paste0("data:image/png;base64,", base64encode(binary))
}

random_control_form <- function(page_id, panel_id, tab_id, random_ctx) {
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

generate_random_demo <- function(params) {
  dist <- sanitize_dist(params$dist)
  n <- sanitize_n(params$n)

  generator <- switch(
    dist,
    norm = rnorm,
    unif = runif,
    lnorm = rlnorm,
    exp = rexp
  )

  values <- generator(n)
  histogram <- generate_histogram_src(values, dist, n)
  summary_lines <- capture.output(summary(values))

  display_n <- min(length(values), 20L)
  table_values <- data.frame(
    Index = seq_len(display_n),
    Value = format(round(values[seq_len(display_n)], 4), nsmall = 4)
  )

  list(
    dist = dist,
    dist_label = random_distribution_labels[[dist]],
    n = n,
    histogram = histogram,
    summary = summary_lines,
    table = table_values,
    query = list(dist = dist, n = n)
  )
}

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

default_selection <- function(page) {
  panels <- page$panels
  first_panel <- names(panels)[1]
  first_tab <- names(panels[[first_panel]]$tabs)[1]
  list(panel = first_panel, tab = first_tab)
}

build_page_navbar <- function(active_page) {
  first_page_id <- names(pages)[1]
  first_defaults <- default_selection(pages[[first_page_id]])

  items <- lapply(names(pages), function(page_id) {
    page <- pages[[page_id]]
    defaults <- default_selection(page)
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
        "Ambiorix Deep Links"
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

set.seed(2025)

traffic_daily <- data.frame(
  date = seq(as.Date("2025-03-01"), by = "day", length.out = 30),
  sessions = sample(950:1600, 30, replace = TRUE),
  conversions = sample(140:280, 30, replace = TRUE)
)
traffic_daily$conversion_rate <- traffic_daily$conversions /
  traffic_daily$sessions

traffic_last7 <- tail(traffic_daily, 7)
traffic_prev7 <- head(traffic_daily, 7)

traffic_metrics <- list(
  list(
    label = "Sessions (30 days)",
    value = format_number(sum(traffic_daily$sessions)),
    trend = trend_info(
      sum(traffic_last7$sessions),
      sum(traffic_prev7$sessions),
      "vs first week"
    )
  ),
  list(
    label = "Avg daily sessions",
    value = format_number(mean(traffic_daily$sessions)),
    trend = trend_info(
      mean(traffic_last7$sessions),
      mean(traffic_prev7$sessions),
      "trailing 7 vs first 7"
    )
  ),
  list(
    label = "Overall conversion rate",
    value = format_percent(
      sum(traffic_daily$conversions) / sum(traffic_daily$sessions)
    ),
    trend = trend_info(
      mean(traffic_last7$conversion_rate),
      mean(traffic_prev7$conversion_rate),
      "change in last week"
    )
  )
)

max_day <- traffic_daily[which.max(traffic_daily$sessions), , drop = FALSE]
traffic_summary <- data.frame(
  Metric = c(
    "Sessions (30 days)",
    "Conversions (30 days)",
    "Conversion rate",
    "Best day"
  ),
  Value = c(
    format_number(sum(traffic_daily$sessions)),
    format_number(sum(traffic_daily$conversions)),
    format_percent(
      sum(traffic_daily$conversions) / sum(traffic_daily$sessions)
    ),
    sprintf(
      "%s (%s sessions)",
      format(max_day$date, "%b %d"),
      format_number(max_day$sessions)
    )
  ),
  check.names = FALSE
)

traffic_recent <- traffic_daily[order(traffic_daily$date, decreasing = TRUE), ][
  1:7,
]
traffic_recent_display <- data.frame(
  Date = format(traffic_recent$date, "%b %d"),
  Sessions = format_number(traffic_recent$sessions),
  Conversions = format_number(traffic_recent$conversions),
  `Conversion rate` = format_percent(traffic_recent$conversion_rate),
  check.names = FALSE
)

campaign_performance <- data.frame(
  Campaign = c(
    "Launch webinar",
    "PositConf recap",
    "Product tour nurture",
    "Retargeting ads"
  ),
  Sessions = c(4210, 3880, 2750, 2140),
  CTR = c(0.034, 0.049, 0.028, 0.062),
  Conversion = c(0.011, 0.023, 0.015, 0.039)
)
campaign_performance_display <- data.frame(
  Campaign = campaign_performance$Campaign,
  Sessions = format_number(campaign_performance$Sessions),
  `Click-through rate` = format_percent(campaign_performance$CTR, digits = 2L),
  `Conversion rate` = format_percent(
    campaign_performance$Conversion,
    digits = 2L
  ),
  check.names = FALSE
)

feature_usage <- data.frame(
  Feature = c("Dashboards", "Exports", "Alerts", "Automations"),
  Weekly_active = c(432, 389, 342, 298),
  WoW = c(0.12, -0.04, 0.07, 0.09),
  Avg_time = c(38, 26, 19, 31)
)
feature_usage_display <- data.frame(
  Feature = feature_usage$Feature,
  `Weekly active users` = format_number(feature_usage$Weekly_active),
  `WoW change` = sprintf("%s", format_percent(feature_usage$WoW, digits = 1L)),
  `Avg. minutes in app` = format_number(feature_usage$Avg_time),
  check.names = FALSE
)

cohort_retention <- data.frame(
  Cohort = c("Jan 2025", "Feb 2025", "Mar 2025"),
  Activation = c(0.67, 0.62, 0.7),
  `Month 2` = c(0.44, 0.39, 0.51),
  `Month 3` = c(0.32, 0.28, 0.46),
  check.names = FALSE
)
cohort_retention_display <- data.frame(
  Cohort = cohort_retention$Cohort,
  `Activation` = format_percent(cohort_retention$Activation, digits = 0L),
  `Month 2` = format_percent(cohort_retention[["Month 2"]], digits = 0L),
  `Month 3` = format_percent(cohort_retention[["Month 3"]], digits = 0L),
  check.names = FALSE
)

incident_history <- data.frame(
  incident_id = paste0("#INC-", 480 + seq_len(12)),
  opened = as.Date("2025-02-01") + sample(0:35, 12),
  severity = sample(
    c("Critical", "High", "Medium"),
    12,
    replace = TRUE,
    prob = c(0.2, 0.5, 0.3)
  ),
  ack_minutes = sample(4:12, 12, replace = TRUE),
  resolve_hours = round(runif(12, min = 1.1, max = 5.5), 1)
)
incident_history <- incident_history[order(incident_history$opened), ]
incident_recent <- tail(incident_history, 6)
incident_baseline <- head(incident_history, nrow(incident_history) - 6)

ack_trend <- if (nrow(incident_baseline) > 0) {
  trend_info(
    median(incident_recent$ack_minutes),
    median(incident_baseline$ack_minutes),
    "vs prior incidents",
    direction = "down-is-good"
  )
} else {
  NULL
}

resolve_trend <- if (nrow(incident_baseline) > 0) {
  trend_info(
    median(incident_recent$resolve_hours),
    median(incident_baseline$resolve_hours),
    "vs prior incidents",
    direction = "down-is-good"
  )
} else {
  NULL
}

incident_metrics <- list(
  list(
    label = "Incidents resolved (30 days)",
    value = format_number(nrow(incident_history)),
    trend = NULL
  ),
  list(
    label = "Median acknowledge time",
    value = sprintf("%.0f min", median(incident_recent$ack_minutes)),
    trend = ack_trend
  ),
  list(
    label = "Median resolve time",
    value = sprintf("%.1f hrs", median(incident_recent$resolve_hours)),
    trend = resolve_trend
  )
)

incident_table_display <- data.frame(
  Incident = incident_recent$incident_id,
  Opened = format(incident_recent$opened, "%b %d"),
  Severity = incident_recent$severity,
  `Ack (min)` = incident_recent$ack_minutes,
  `Resolve (hrs)` = sprintf("%.1f", incident_recent$resolve_hours),
  check.names = FALSE
)

onboarding_checklist_display <- data.frame(
  Step = c(
    "Access ready",
    "Shadow two customer calls",
    "Ship first change",
    "Share learning at retro"
  ),
  Owner = c("IT Ops", "Success lead", "Mentor", "New hire"),
  SLA = c("Day 0", "Week 1", "Week 2", "Week 2"),
  Resource = c(
    "Access request tracker",
    "Recordings folder",
    "Starter backlog",
    "Retro template"
  )
)

review_schedule_display <- data.frame(
  Ritual = c("Quarterly business review", "Monthly retro", "Ops health sync"),
  Cadence = c("Quarterly", "Monthly", "Bi-weekly"),
  `Next date` = format(
    as.Date(c("2025-04-04", "2025-03-28", "2025-03-14")),
    "%b %d"
  ),
  Owner = c("Alex", "Tari", "Olivia"),
  check.names = FALSE
)

pages <- list(
  welcome = list(
    title = "Shareable Home",
    tagline = "Give newcomers the map: pages, panels, and tabs each earn a stable route.",
    panels = list(
      orientation = list(
        title = "Orientation",
        intro = "Unpack Ambiorix navigation layers and how they translate into routes.",
        tabs = list(
          tour = list(
            title = "Layout tour",
            body = tagList(
              tags$p(
                "Each view in this demo nests like classic tabsets—page → panel → tab. ",
                "Ambiorix mirrors that structure in the URL so you can share the exact context."
              ),
              tags$ul(
                tags$li(
                  "Pages anchor the high-level experience (e.g. analytics, operations)."
                ),
                tags$li(
                  "Panels group related stories within the page (e.g. traffic, engagement)."
                ),
                tags$li(
                  "Tabs zoom into the data narrative you want to highlight."
                )
              ),
              tags$div(
                class = "card border-0 shadow-sm mt-4",
                tags$div(
                  class = "card-body",
                  tags$h3(class = "h5 mb-3", "Sample analytics snapshot"),
                  bootstrap_table(
                    traffic_summary,
                    caption = "Synthetic data powering the analytics page"
                  )
                )
              )
            )
          ),
          recap = list(
            title = "Why links matter",
            body = tagList(
              tags$p(
                "PositConf 2025 attendees asked how to share the exact tab they were demoing. ",
                "With Ambiorix, the answer is simply to pass along the path."
              ),
              tags$p(
                "Try copying the link card from any analytics tab to see the full route down to nested content."
              ),
              tags$blockquote(
                class = "blockquote",
                tags$p(
                  "Because routes are just strings, you can drop them into doc sites, slide decks, or chat threads."
                ),
                tags$footer(
                  class = "blockquote-footer",
                  "Product tour rehearsal notes"
                )
              )
            )
          )
        )
      ),
      recipes = list(
        title = "Link recipes",
        intro = "Patterns you can lift directly into your own apps.",
        tabs = list(
          anatomy = list(
            title = "URL anatomy",
            body = tagList(
              tags$p("Routes compose as `/pages/<page>/<panel>/<tab>`."),
              tags$pre(
                class = "bg-light rounded p-3 border",
                "# Example\n/pages/analytics/traffic/campaigns"
              ),
              tags$p(
                "Because each segment maps to the nested tabsets, you can bookmark any level. ",
                "Need the whole Traffic panel? Stop at `/pages/analytics/traffic`."
              )
            )
          ),
          onboarding = list(
            title = "Onboarding tip",
            body = tagList(
              tags$p(
                "Slide these links into your runbooks so data folks land inside familiar tab panels without any warm-up."
              ),
              tags$blockquote(
                class = "blockquote",
                tags$p(
                  "Share the `/pages/operations/playbooks/incident-response` link during drills to open the checklist instantly."
                ),
                tags$footer(class = "blockquote-footer", "Reliability weekly")
              )
            )
          )
        )
      )
    )
  ),
  analytics = list(
    title = "Analytics",
    tagline = "Showcase traffic, campaign, and engagement data with deep links for every view.",
    panels = list(
      traffic = list(
        title = "Traffic",
        intro = "Synthetic 30-day dataset to mimic a polished KPI dashboard.",
        tabs = list(
          overview = list(
            title = "Overview",
            body = tagList(
              tags$p("Headline metrics for the last 30 days."),
              metric_cards(traffic_metrics),
              tags$div(
                class = "card border-0 shadow-sm mt-4",
                tags$div(
                  class = "card-body",
                  tags$h3(class = "h5 mb-3", "30-day snapshot"),
                  bootstrap_table(traffic_summary)
                )
              )
            )
          ),
          trend = list(
            title = "Daily trend",
            body = tagList(
              tags$p(
                "Seven most recent days—shareable straight from this nested tab."
              ),
              tags$div(
                class = "card border-0 shadow-sm",
                tags$div(
                  class = "card-body",
                  bootstrap_table(
                    traffic_recent_display,
                    caption = "Last 7 days of synthetic traffic"
                  ),
                  tags$p(
                    class = "text-muted small mt-3",
                    "Perfect for teammates asking for the latest daily numbers."
                  )
                )
              )
            )
          ),
          campaigns = list(
            title = "Campaigns",
            body = tagList(
              tags$p(
                "Campaign performance with click-through and conversion rates."
              ),
              tags$div(
                class = "card border-0 shadow-sm",
                tags$div(
                  class = "card-body",
                  bootstrap_table(campaign_performance_display)
                )
              ),
              tags$p(
                class = "text-muted small mt-3",
                "Use `/pages/analytics/traffic/campaigns` in follow-up emails to jump straight here."
              )
            )
          )
        )
      ),
      engagement = list(
        title = "Engagement",
        intro = "Follow how activated users behave across features and cohorts.",
        tabs = list(
          features = list(
            title = "Feature usage",
            body = tagList(
              tags$p("Weekly active counts and time in app for key features."),
              tags$div(
                class = "card border-0 shadow-sm",
                tags$div(
                  class = "card-body",
                  bootstrap_table(feature_usage_display)
                )
              )
            )
          ),
          cohorts = list(
            title = "Cohorts",
            body = tagList(
              tags$p(
                "Activation and retention percentages by acquisition cohort."
              ),
              tags$div(
                class = "card border-0 shadow-sm",
                tags$div(
                  class = "card-body",
                  bootstrap_table(cohort_retention_display)
                )
              ),
              tags$p(
                class = "text-muted small mt-3",
                "Share `/pages/analytics/engagement/cohorts` when discussing retention initiatives."
              )
            )
          )
        )
      )
    )
  ),
  simulations = list(
    title = "Simulations",
    tagline = "Recreate a classroom-style random distribution explorer with shareable routes.",
    panels = list(
      random = list(
        title = "Random explorer",
        intro = "Pick a distribution and sample size; each nested tab mirrors a familiar tabbed exploration flow.",
        tabs = list(
          plot = list(
            title = "Plot",
            body = function(req, ctx) {
              random <- ctx$random %||% generate_random_demo(ctx$params)
              tags$div(
                random_control_form(
                  ctx$page_id,
                  ctx$panel_id,
                  ctx$tab_id,
                  random
                ),
                tags$div(
                  class = "card border-0 shadow-sm mt-4",
                  tags$div(
                    class = "card-body",
                    tags$h3(
                      class = "h5 mb-3",
                      sprintf(
                        "Histogram · %s · n = %s",
                        random$dist_label,
                        random$n
                      )
                    ),
                    tags$img(
                      src = random$histogram,
                      class = "img-fluid rounded border",
                      alt = sprintf(
                        "Histogram of %s samples",
                        random$dist_label
                      )
                    ),
                    tags$p(
                      class = "text-muted small mt-3",
                      "Hit copy in the share card to send teammates straight to this distribution state."
                    )
                  )
                )
              )
            }
          ),
          summary = list(
            title = "Summary",
            body = function(req, ctx) {
              random <- ctx$random %||% generate_random_demo(ctx$params)
              tags$div(
                random_control_form(
                  ctx$page_id,
                  ctx$panel_id,
                  ctx$tab_id,
                  random
                ),
                tags$div(
                  class = "card border-0 shadow-sm mt-4",
                  tags$div(
                    class = "card-body",
                    tags$h3(
                      class = "h5 mb-3",
                      sprintf("Five-number summary · %s", random$dist_label)
                    ),
                    tags$pre(
                      class = "bg-light rounded p-3",
                      paste(random$summary, collapse = "\n")
                    ),
                    tags$p(
                      class = "text-muted small mt-3",
                      "Shareable route: submit the form, then copy the link card—colleagues land on the same summary."
                    )
                  )
                )
              )
            }
          ),
          table = list(
            title = "Table",
            body = function(req, ctx) {
              random <- ctx$random %||% generate_random_demo(ctx$params)
              tags$div(
                random_control_form(
                  ctx$page_id,
                  ctx$panel_id,
                  ctx$tab_id,
                  random
                ),
                tags$div(
                  class = "card border-0 shadow-sm mt-4",
                  tags$div(
                    class = "card-body",
                    tags$h3(
                      class = "h5 mb-3",
                      sprintf("First %s simulated values", nrow(random$table))
                    ),
                    bootstrap_table(
                      random$table,
                      class = "table table-sm table-hover align-middle mb-0"
                    ),
                    tags$p(
                      class = "text-muted small mt-3",
                      "Use this tab to share reproducible raw values for debugging or demos."
                    )
                  )
                )
              )
            }
          )
        )
      )
    )
  ),
  operations = list(
    title = "Operations",
    tagline = "Operational data and checklists with resolvable deep links.",
    panels = list(
      playbooks = list(
        title = "Playbooks",
        intro = "Keep the incident squad and onboarding team aligned.",
        tabs = list(
          incident = list(
            title = "Incident response",
            body = tagList(
              tags$p("Latest incidents with acknowledge and resolve metrics."),
              metric_cards(incident_metrics),
              tags$div(
                class = "card border-0 shadow-sm mt-4",
                tags$div(
                  class = "card-body",
                  tags$h3(class = "h5 mb-3", "Most recent incidents"),
                  bootstrap_table(incident_table_display)
                )
              )
            )
          ),
          onboarding = list(
            title = "New hire onboarding",
            body = tagList(
              tags$p("Checklist to help new teammates ramp quickly."),
              tags$div(
                class = "card border-0 shadow-sm",
                tags$div(
                  class = "card-body",
                  bootstrap_table(onboarding_checklist_display)
                )
              ),
              tags$p(
                class = "text-muted small mt-3",
                "Link `/pages/operations/playbooks/onboarding` in welcome emails."
              )
            )
          )
        )
      ),
      reviews = list(
        title = "Reviews",
        intro = "Recurring rituals, ready to share with stakeholders.",
        tabs = list(
          quarterly = list(
            title = "Quarterly review",
            body = tagList(
              tags$p("Who owns what and when the next review lands."),
              tags$div(
                class = "card border-0 shadow-sm",
                tags$div(
                  class = "card-body",
                  bootstrap_table(review_schedule_display)
                )
              )
            )
          ),
          retros = list(
            title = "Retrospective",
            body = tagList(
              tags$p(
                "Remind the team how to prep asynchronously before retro."
              ),
              tags$blockquote(
                class = "blockquote",
                tags$p(
                  "Drop the link in Slack ahead of time so notes collect in the same spot."
                ),
                tags$footer(class = "blockquote-footer", "Ops lead")
              ),
              tags$p(
                "Route: ",
                tags$code("/pages/operations/reviews/retros")
              )
            )
          )
        )
      )
    )
  )
)

build_panel_nav <- function(page_id, active_panel, query = NULL) {
  page <- pages[[page_id]]

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

build_tab_nav <- function(page_id, panel_id, active_tab, query = NULL) {
  panel <- pages[[page_id]]$panels[[panel_id]]

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

build_breadcrumb <- function(page_id, panel_id, tab_id) {
  page <- pages[[page_id]]
  panel <- page$panels[[panel_id]]
  tab <- panel$tabs[[tab_id]]

  page_defaults <- default_selection(page)
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

build_tab_content <- function(page_id, panel_id, tab_id, req, context) {
  panel <- pages[[page_id]]$panels[[panel_id]]
  tab <- panel$tabs[[tab_id]]
  intro <- panel$intro %||% ""

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

build_share_card <- function(req, page_id, panel_id, tab_id, query = NULL) {
  scheme <- req$rook.url_scheme %||% "http"
  host <- req$HTTP_HOST %||% req$SERVER_NAME %||% "localhost"
  relative <- compose_href(path_for(page_id, panel_id, tab_id), query)
  absolute <- paste0(scheme, "://", host, relative)

  tags$div(
    class = "card bg-dark text-white border-0",
    tags$div(
      class = "card-body",
      tags$h2(class = "h5 mb-3", "Share this exact view"),
      tags$p(
        class = "mb-3 text-light",
        "Copy the link below—anyone opening it lands directly in this nested tab."
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

render_page <- function(req, res, page_id, panel_id, tab_id) {
  page <- pages[[page_id]]

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

  navbar <- build_page_navbar(page_id)
  panel_nav <- build_panel_nav(page_id, panel_id, query_for_nav)
  tab_nav <- build_tab_nav(page_id, panel_id, tab_id, query_for_nav)
  breadcrumb <- build_breadcrumb(page_id, panel_id, tab_id)
  tab_content <- build_tab_content(page_id, panel_id, tab_id, req, context)
  share_card <- build_share_card(req, page_id, panel_id, tab_id, query_for_nav)

  html <- tags$html(
    lang = "en",
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      tags$title(sprintf("%s · Ambiorix Deep Links", page$title)),
      tags$link(
        rel = "stylesheet",
        href = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css"
      ),
      tags$link(rel = "stylesheet", href = "/static/main.css"),
    ),
    tags$body(
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
      ),
      tags$script(
        src = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/js/bootstrap.bundle.min.js"
      )
    )
  )

  res$send(html)
}

app <- Ambiorix$new()

default_page <- names(pages)[1]
default_target <- default_selection(pages[[default_page]])
default_path <- path_for(default_page, default_target$panel, default_target$tab)

app$get("/", \(req, res) {
  res$status <- 302L
  res$redirect(default_path)
})

pages_router <- Router$new("/pages")

pages_router$get("/", \(req, res) {
  res$status <- 302L
  res$redirect(default_path)
})

pages_router$get("/:page", \(req, res) {
  page_id <- req$params$page

  if (!page_id %in% names(pages)) {
    res$status <- 302L
    return(
      res$redirect(default_path)
    )
  }

  defaults <- default_selection(pages[[page_id]])
  res$status <- 302L
  res$redirect(path_for(page_id, defaults$panel, defaults$tab))
})

pages_router$get("/:page/:panel", \(req, res) {
  page_id <- req$params$page
  panel_id <- req$params$panel

  if (!page_id %in% names(pages)) {
    res$status <- 302L
    return(res$redirect(default_path))
  }

  page <- pages[[page_id]]

  if (!panel_id %in% names(page$panels)) {
    defaults <- default_selection(page)
    panel_id <- defaults$panel
  }

  default_tab <- names(page$panels[[panel_id]]$tabs)[1]
  res$status <- 302L
  res$redirect(path_for(page_id, panel_id, default_tab))
})

pages_router$get("/:page/:panel/:tab", \(req, res) {
  page_id <- req$params$page
  panel_id <- req$params$panel
  tab_id <- req$params$tab

  if (!page_id %in% names(pages)) {
    res$status <- 302L
    return(
      res$redirect(default_path)
    )
  }

  page <- pages[[page_id]]

  if (!panel_id %in% names(page$panels)) {
    defaults <- default_selection(page)
    res$status <- 302L
    return(
      res$redirect(
        path_for(page_id, defaults$panel, defaults$tab)
      )
    )
  }

  panel <- page$panels[[panel_id]]

  if (!tab_id %in% names(panel$tabs)) {
    default_tab <- names(panel$tabs)[1]
    res$status <- 302L
    return(
      res$redirect(
        path_for(page_id, panel_id, default_tab)
      )
    )
  }

  render_page(req, res, page_id, panel_id, tab_id)
})

app$use(pages_router)
app$static("public", "static")

app$start(port = 3000L)
