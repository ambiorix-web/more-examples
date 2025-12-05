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
#' @export
UI <- function() {
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
          action = "/upload",
          enctype = "multipart/form-data",
          method = "post",
          FileInput(
            id = "file",
            label = "CSV file",
            accept = ".csv, text/csv"
          ),
          tags$button(
            id = "upload",
            type = "submit",
            class = "btn btn-sm btn-dark mt-3 d-none",
            "Upload"
          )
        )
      )
    ),
    tags$script(src = "/static/main.js")
  )

  Page(content)
}
