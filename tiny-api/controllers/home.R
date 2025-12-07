box::use(
  data.table[fread],
  ambiorix[parse_multipart],
  uuid[UUIDgenerate],
  htmltools[
    tags,
    tagList,
  ],
  .. / ui / home[UI],
  .. /
    db[
      read_table,
      create_table,
    ],
)

#' Handle GET at '/'
#'
#' @export
home_get <- function(req, res) {
  res$send(UI())
}

#' Handle POST at '/'
#'
#' @export
home_post <- function(req, res) {
  data <- parse_multipart(req)
  csv_details <- data$file

  if (!identical(csv_details$content_type, "text/csv")) {
    html <- UI(
      tags$p(
        class = "text-danger fw-bold",
        "File must be a csv file"
      )
    )

    return(
      res$send(html)
    )
  }

  max_size <- 5 * 1024 * 1024 # 5mb
  if (length(csv_details$value) > max_size) {
    html <- UI(
      tags$p(
        class = "text-danger fw-bold",
        "File must be less than 5MB"
      )
    )

    return(
      res$send(html)
    )
  }

  csv_data <- rawToChar(csv_details$value) |> fread()
  table_name <- UUIDgenerate()

  create_table(name = table_name, data = csv_data)

  href <- sprintf("/%s", table_name)

  html <- UI(
    tags$div(
      class = "alert alert-success",
      role = "alert",
      "Success. Find your data in JSON format",
      tags$a(
        href = href,
        "here"
      )
    )
  )

  res$send(html)
}

#' Handler for GET at '/:table'
#'
#' @export
table_get <- function(req, res) {
  name <- req$params$table
  data <- read_table(name = name)

  response <- list(
    msg = "success",
    data = data
  )

  res$json(response)
}
