box::use(
  htmltools[
    tags,
    tagList,
    HTML,
  ],
  . / page[Page],
  . /
    components /
    card[
      Card,
    ],
)

#' Docs page UI
#'
#' @export
UI <- function() {
  content <- tags$div(
    class = "container my-5",
    tags$h1(
      class = "mb-4",
      "Tiny API"
    ),
    tags$p(
      class = "lead mb-5",
      "Upload data, get API endpoints. Nothing else."
    ),

    section(
      title = "Overview",
      tags$p(
        "Tiny API lets you upload CSV files and instantly get ",
        "JSON API endpoints to access your data. No configuration, ",
        "no database setup."
      ),
      tags$ol(
        tags$li("Upload a CSV file (max 5MB)"),
        tags$li("Get a unique endpoint URL"),
        tags$li("Fetch your data as JSON")
      )
    ),

    section(
      title = "Endpoints",
      endpoint(
        method = "GET",
        path = "/",
        description = "Home page with file upload form."
      ),
      endpoint(
        method = "POST",
        path = "/",
        description = "Upload a CSV file. Returns a redirect to the home page with a link to your data.",
        params = list(
          list(
            name = "file",
            type = "multipart/form-data",
            desc = "CSV file (max 5MB)"
          )
        )
      ),
      endpoint(
        method = "GET",
        path = "/tables",
        description = "List all available tables.",
        example_response = '{"msg": "Success", "data": ["uuid-1", "uuid-2"]}'
      ),
      endpoint(
        method = "GET",
        path = "/:table",
        description = "Retrieve data from a table.",
        params = list(
          list(
            name = "limit",
            type = "query",
            desc = "Max number of rows to return (optional)"
          ),
          list(
            name = "offset",
            type = "query",
            desc = "Number of rows to skip (optional)"
          )
        ),
        example_response = '{"msg": "Success", "data": [...]}'
      ),
      endpoint(
        method = "DELETE",
        path = "/:table",
        description = "Delete a table.",
        example_response = '{"msg": "Success. Table deleted."}'
      )
    ),

    section(
      title = "Examples",
      tags$h5("Upload a file"),
      code_block('curl -X POST -F "file=@data.csv" http://localhost:3000/'),
      tags$h5(class = "mt-4", "List all tables"),
      code_block("curl http://localhost:3000/tables"),
      tags$h5(class = "mt-4", "Get data"),
      code_block("curl http://localhost:3000/{table-uuid}"),
      tags$h5(class = "mt-4", "Get data with pagination"),
      code_block("curl \"http://localhost:3000/{table-uuid}?limit=10&offset=0\""),
      tags$h5(class = "mt-4", "Delete a table"),
      code_block("curl -X DELETE http://localhost:3000/{table-uuid}")
    ),

    section(
      title = "Error Responses",
      tags$p("When a table is not found:"),
      code_block('{"msg": "Failed. Not found."}', lang = "json"),
      tags$p(class = "mt-3", "HTTP status code: ", tags$code("404"))
    )
  )

  Page(content, title = "Docs | Tiny API")
}

#' Section helper
#'
#' @param title String. Section title.
#' @param ... Content.
section <- function(title, ...) {
  Card(
    class = "border-0 mb-4 shadow-sm",
    body_class = "p-4",
    title = title,
    title_class = "mb-3",
    ...
  )
}

#' Endpoint documentation helper
#'
#' @param method String. HTTP method.
#' @param path String. Endpoint path.
#' @param description String. Description.
#' @param params List. Optional parameters.
#' @param example_response String. Optional example response.
endpoint <- function(
  method,
  path,
  description,
  params = NULL,
  example_response = NULL
) {
  method_class <- switch(
    method,
    GET = "bg-success",
    POST = "bg-primary",
    DELETE = "bg-danger",
    "bg-secondary"
  )

  tagList(
    tags$div(
      class = "d-flex align-items-center mb-2 mt-3",
      tags$span(
        class = paste("badge", method_class, "me-2"),
        method
      ),
      tags$code(path)
    ),
    tags$p(class = "mb-2", description),
    if (!is.null(params)) {
      tags$ul(
        class = "mb-2",
        lapply(params, function(p) {
          tags$li(
            tags$code(p$name),
            paste0(" (", p$type, "): "),
            p$desc
          )
        })
      )
    },
    if (!is.null(example_response)) {
      code_block(example_response, lang = "json")
    }
  )
}

#' Code block helper
#'
#' @param code String. Code content.
#' @param lang String. Language (for styling).
code_block <- function(code, lang = "bash") {
  tags$pre(
    class = "bg-dark text-light p-3 rounded",
    tags$code(code)
  )
}
