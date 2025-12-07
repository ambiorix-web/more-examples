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

    Section(
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

    Section(
      title = "Endpoints",
      Endpoint(
        method = "GET",
        path = "/",
        description = "Home page with file upload form."
      ),
      Endpoint(
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
      Endpoint(
        method = "GET",
        path = "/tables",
        description = "List all available tables.",
        example_response = '{"msg": "Success", "data": ["uuid-1", "uuid-2"]}'
      ),
      Endpoint(
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
      Endpoint(
        method = "DELETE",
        path = "/:table",
        description = "Delete a table.",
        example_response = '{"msg": "Success. Table deleted."}'
      )
    ),

    Section(
      title = "Examples",
      tags$h5("Upload a file"),
      CodeBlock('curl -X POST -F "file=@data.csv" http://localhost:3000/'),
      tags$h5(class = "mt-4", "List all tables"),
      CodeBlock("curl http://localhost:3000/tables"),
      tags$h5(class = "mt-4", "Get data"),
      CodeBlock("curl http://localhost:3000/{table-uuid}"),
      tags$h5(class = "mt-4", "Get data with pagination"),
      CodeBlock(
        "curl \"http://localhost:3000/{table-uuid}?limit=10&offset=0\""
      ),
      tags$h5(class = "mt-4", "Delete a table"),
      CodeBlock("curl -X DELETE http://localhost:3000/{table-uuid}")
    ),

    Section(
      title = "Error Responses",
      tags$p("When a table is not found:"),
      CodeBlock('{"msg": "Failed. Not found."}', lang = "json"),
      tags$p(class = "mt-3", "HTTP status code: ", tags$code("404"))
    )
  )

  Page(content, title = "Docs | Tiny API")
}

#' Section
#'
#' @param title String /// Required. Section title.
#' @param ... [htmltools::tags] /// Optional. Content.
#'
#' @return [Card()]
Section <- function(title, ...) {
  Card(
    class = "border-0 mb-4 shadow-sm",
    body_class = "p-4",
    title = title,
    title_class = "mb-3",
    ...
  )
}

#' Document an Endpoint
#'
#' @param method String /// Optional. HTTP method.
#'        Valid values are:
#'        - "GET" (default)
#'        - "POST"
#'        - "DELETE"
#' @param path String /// Required. Endpoint path.
#' @param description String /// Required. Description.
#' @param params List /// Optional. Parameters.
#' @param example_response String /// Optional. Example response.
#'
#' @return [htmltools::tags]
Endpoint <- function(
  method = c("GET", "POST", "DELETE"),
  path,
  description,
  params = NULL,
  example_response = NULL
) {
  method <- match.arg(arg = method)
  method_class <- switch(
    method,
    GET = "bg-success",
    POST = "bg-primary",
    DELETE = "bg-danger"
  )

  params_html <- NULL
  if (!is.null(params)) {
    params_html <- tags$ul(
      class = "mb-2",
      lapply(params, function(p) {
        tags$li(
          tags$code(p$name),
          paste0(" (", p$type, "): "),
          p$desc
        )
      })
    )
  }

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
    params_html,

    if (!is.null(example_response)) {
      CodeBlock(example_response, lang = "json")
    }
  )
}

#' Code Block
#'
#' @param code String /// Required. Code content.
#' @param lang String /// Optional. Language (for styling).
#'        Defaults to "bash".
#'
#' @return [htmltools::tags$pre]
CodeBlock <- function(code, lang = "bash") {
  tags$pre(
    class = "bg-dark text-light p-3 rounded",
    tags$code(code)
  )
}
