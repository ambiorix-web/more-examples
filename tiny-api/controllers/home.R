box::use(
  data.table[fread],
  ambiorix[parse_multipart],
  uuid[UUIDgenerate],
  htmltools[
    tags,
    tagList,
  ],
  .. / ui / home[UI],
  .. / ui / docs[DocsUI = UI],
  .. /
    db[
      read_table,
      create_table,
      delete_table,
      list_tables,
      table_exists,
    ],
  .. / utils / create_href[create_href],
)

#' Handle GET at '/'
#'
#' @export
home_get <- function(req, res) {
  link <- req$query$link
  link_html <- NULL

  if (!is.null(link) && !is.na(link)) {
    link_html <- tags$div(
      class = "alert alert-success",
      role = "alert",
      "Success. Find your data in JSON format",
      tags$a(
        href = create_href(
          href = paste0("/", link)
        ),
        "here."
      )
    )
  }

  html <- UI(link_html)

  res$send(html)
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

  path <- create_href(
    href = sprintf("/?link=%s", table_name)
  )

  res$status <- 302L
  res$redirect(path = path)
}

#' Handler for GET at '/:table'
#'
#' @export
table_get <- function(req, res) {
  name <- req$params$table

  if (!table_exists(name = name)) {
    res$status <- 404L

    return(
      res$json(list(
        msg = "Failed. Not found."
      ))
    )
  }

  limit <- req$query$limit
  offset <- req$query$offset

  if (!is.null(limit)) {
    limit <- as.integer(limit)
  }

  if (!is.null(offset)) {
    offset <- as.integer(offset)
  }

  data <- read_table(name = name, limit = limit, offset = offset)

  response <- list(
    msg = "Success",
    data = data
  )

  res$json(response)
}

#' Handler for DELETE at '/:table'
#'
#' @export
table_delete <- function(req, res) {
  name <- req$params$table

  if (!table_exists(name = name)) {
    res$status <- 404L

    return(
      res$json(list(
        msg = "Failed. Not found."
      ))
    )
  }

  delete_table(name = name)

  res$json(list(
    msg = "Success. Table deleted."
  ))
}

#' Handler for GET at '/tables'
#'
#' @export
tables_get <- function(req, res) {
  tables <- list_tables()

  response <- list(
    msg = "Success",
    data = tables
  )

  res$json(response)
}

#' Handler for GET at '/docs'
#'
#' @export
docs_get <- function(req, res) {
  html <- DocsUI()

  res$send(html)
}
