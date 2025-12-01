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
  content <- tags$div(
    class = "container",
    Card(
      class = "border-0 my-5 shadow-sm",
      body_class = "p-4",
      title = "Tiny API",
      title_class = "mb-4",
      tags$p(
        "Upload data, get API endpoints. Nothing else."
      ),
      FileInput(
        id = "file",
        label = "CSV file"
      )
    )
  )

  Page(content)
}
