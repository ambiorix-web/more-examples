box::use(
  htmltools[
    tags,
    tagList,
  ],
  . / datasets[
    traffic_metrics,
    traffic_summary,
    traffic_recent_display,
    campaign_performance_display,
    feature_usage_display,
    cohort_retention_display,
    incident_metrics,
    incident_table_display,
    onboarding_checklist_display,
    review_schedule_display,
  ],
  . / random[
    random_distribution_labels,
    generate_random_demo,
  ],
  .. /
    utils /
    operators[
      `%||%`,
    ],
  .. /
    ui /
    components[
      MetricCards,
      BootstrapTable,
      RandomControlForm,
    ],
  .. /
    ui /
    navigation[
      path_for,
    ],
)

#' @export
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
                "Each view in this demo nests like classic tabsets-page -> panel -> tab. ",
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
                  BootstrapTable(
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
              MetricCards(traffic_metrics),
              tags$div(
                class = "card border-0 shadow-sm mt-4",
                tags$div(
                  class = "card-body",
                  tags$h3(class = "h5 mb-3", "30-day snapshot"),
                  BootstrapTable(traffic_summary)
                )
              )
            )
          ),
          trend = list(
            title = "Daily trend",
            body = tagList(
              tags$p(
                "Seven most recent days-shareable straight from this nested tab."
              ),
              tags$div(
                class = "card border-0 shadow-sm",
                tags$div(
                  class = "card-body",
                  BootstrapTable(
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
                  BootstrapTable(campaign_performance_display)
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
                  BootstrapTable(feature_usage_display)
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
                  BootstrapTable(cohort_retention_display)
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
                RandomControlForm(
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
                        "Histogram - %s - n = %s",
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
                RandomControlForm(
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
                      sprintf("Five-number summary - %s", random$dist_label)
                    ),
                    tags$pre(
                      class = "bg-light rounded p-3",
                      paste(random$summary, collapse = "\n")
                    ),
                    tags$p(
                      class = "text-muted small mt-3",
                      "Shareable route: submit the form, then copy the link card-colleagues land on the same summary."
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
                RandomControlForm(
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
                    BootstrapTable(
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
              MetricCards(incident_metrics),
              tags$div(
                class = "card border-0 shadow-sm mt-4",
                tags$div(
                  class = "card-body",
                  tags$h3(class = "h5 mb-3", "Most recent incidents"),
                  BootstrapTable(incident_table_display)
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
                  BootstrapTable(onboarding_checklist_display)
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
                  BootstrapTable(review_schedule_display)
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

#' Get default panel and tab selection for a page
#'
#' @param page Page definition list.
#'
#' @return List with panel and tab names.
#' @export
default_selection <- function(page) {
  panels <- page$panels
  first_panel <- names(panels)[1]
  first_tab <- names(panels[[first_panel]]$tabs)[1]
  list(panel = first_panel, tab = first_tab)
}
