box::use(
  htmltools[
    tags,
    tagList,
  ],
  . / page[Page],
  . /
    components /
    file_input[
      FileInput
    ],
  . /
    components /
    card[
      Card,
    ],
)

#' Home page UI
#'
#' @param ... [htmltools::tags] /// Optional. Tags to insert
#'        after the home card.
#' @export
UI <- function(...) {
  content <- tagList(
    tags$div(
      class = "container",
      Card(
        class = "border-0 my-5 shadow-sm",
        body_class = "p-4",
        title = "Tiny API",
        title_class = "mb-4",
        tags$p(
          "Upload data, get API endpoints. Nothing else."
        ),
        tags$form(
          action = "/",
          enctype = "multipart/form-data",
          method = "post",
          FileInput(
            id = "file",
            label = tags$p(
              class = "mb-0",
              "CSV file",
              tags$small(
                class = "fw-bold",
                " (< 5MB)"
              )
            ),
            accept = ".csv, text/csv"
          ),
          tags$button(
            id = "upload",
            type = "submit",
            class = "btn btn-sm btn-dark mt-3 d-none",
            "Upload"
          )
        )
      ),
      ...
    ),
    tags$script(src = "/static/main.js")
  )

  Page(content)
}
