box::use(
  stats[
    runif,
    median,
  ],
  utils[
    tail,
    head,
  ],
  . /
    helpers[
      format_number,
      format_percent,
      trend_info,
    ],
)

set.seed(2025)

# Traffic data
traffic_daily <- data.frame(
  date = seq(as.Date("2025-03-01"), by = "day", length.out = 30),
  sessions = sample(950:1600, 30, replace = TRUE),
  conversions = sample(140:280, 30, replace = TRUE)
)
traffic_daily$conversion_rate <- traffic_daily$conversions /
  traffic_daily$sessions

traffic_last7 <- tail(traffic_daily, 7)
traffic_prev7 <- head(traffic_daily, 7)

#' @export
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

#' @export
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

#' @export
traffic_recent_display <- data.frame(
  Date = format(traffic_recent$date, "%b %d"),
  Sessions = format_number(traffic_recent$sessions),
  Conversions = format_number(traffic_recent$conversions),
  `Conversion rate` = format_percent(traffic_recent$conversion_rate),
  check.names = FALSE
)

# Campaign data
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

#' @export
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

# Feature usage data
feature_usage <- data.frame(
  Feature = c("Dashboards", "Exports", "Alerts", "Automations"),
  Weekly_active = c(432, 389, 342, 298),
  WoW = c(0.12, -0.04, 0.07, 0.09),
  Avg_time = c(38, 26, 19, 31)
)

#' @export
feature_usage_display <- data.frame(
  Feature = feature_usage$Feature,
  `Weekly active users` = format_number(feature_usage$Weekly_active),
  `WoW change` = sprintf("%s", format_percent(feature_usage$WoW, digits = 1L)),
  `Avg. minutes in app` = format_number(feature_usage$Avg_time),
  check.names = FALSE
)

# Cohort data
cohort_retention <- data.frame(
  Cohort = c("Jan 2025", "Feb 2025", "Mar 2025"),
  Activation = c(0.67, 0.62, 0.7),
  `Month 2` = c(0.44, 0.39, 0.51),
  `Month 3` = c(0.32, 0.28, 0.46),
  check.names = FALSE
)

#' @export
cohort_retention_display <- data.frame(
  Cohort = cohort_retention$Cohort,
  `Activation` = format_percent(cohort_retention$Activation, digits = 0L),
  `Month 2` = format_percent(cohort_retention[["Month 2"]], digits = 0L),
  `Month 3` = format_percent(cohort_retention[["Month 3"]], digits = 0L),
  check.names = FALSE
)

# Incident data
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

#' @export
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

#' @export
incident_table_display <- data.frame(
  Incident = incident_recent$incident_id,
  Opened = format(incident_recent$opened, "%b %d"),
  Severity = incident_recent$severity,
  `Ack (min)` = incident_recent$ack_minutes,
  `Resolve (hrs)` = sprintf("%.1f", incident_recent$resolve_hours),
  check.names = FALSE
)

#' @export
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

#' @export
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
