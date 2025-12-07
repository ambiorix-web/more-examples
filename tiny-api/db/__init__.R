box::use(
  DBI[
    dbConnect,
    dbReadTable,
    dbWriteTable,
    dbDisconnect,
    dbExistsTable,
    dbRemoveTable,
    dbListTables,
  ],
  data.table[
    data.table,
    as.data.table,
  ],
  RSQLite[
    SQLite,
  ],
  .. / root[file_path],
)

#' Connect To DB
#'
#' @export
db_connect <- function() {
  dbConnect(
    drv = SQLite(),
    dbname = file_path("tiny-api.sqlite")
  )
}

#' Create New Table
#'
#' @param name String /// Required. Name of the table.
#' @param data data.table /// Required. Table data.
#'
#' @return `TRUE` (invisibly)
#'
#' @export
create_table <- function(name, data) {
  con <- db_connect()
  on.exit(dbDisconnect(conn = con))

  dbWriteTable(conn = con, name = name, value = data)

  invisible(TRUE)
}

#' Delete Table
#'
#' @param name String /// Required. Name of the table.
#'
#' @return `TRUE` (invisibly)
#'
#' @export
delete_table <- function(name) {
  con <- db_connect()
  on.exit(dbDisconnect(conn = con))

  dbRemoveTable(conn = con, name = name)

  invisible(TRUE)
}

#' Read Table
#'
#' @param name String /// Required. Name of the table.
#' @param limit Integer /// Optional. Max rows to return.
#' @param offset Integer /// Optional. Rows to skip.
#'
#' @return data.table. If a table with `name` is not found,
#'         returns a 0-row data.table.
#'
#' @export
read_table <- function(name, limit = NULL, offset = NULL) {
  con <- db_connect()
  on.exit(dbDisconnect(conn = con))

  if (!dbExistsTable(conn = con, name = name)) {
    return(
      data.table()
    )
  }

  data <- dbReadTable(conn = con, name = name) |>
    as.data.table()

  if (!is.null(offset) && offset <= nrow(data)) {
    data <- data[-(1:offset)]
  }

  if (!is.null(limit) && limit <= nrow(data)) {
    data <- data[1:limit]
  }

  data
}

#' List All Tables
#'
#' @return Character vector of table names.
#'
#' @export
list_tables <- function() {
  con <- db_connect()
  on.exit(dbDisconnect(conn = con))

  dbListTables(conn = con)
}

#' Check If Table Exists
#'
#' @param name String /// Required. Name of the table.
#'
#' @return Logical
#'
#' @export
table_exists <- function(name) {
  con <- db_connect()
  on.exit(dbDisconnect(conn = con))

  dbExistsTable(conn = con, name = name)
}
