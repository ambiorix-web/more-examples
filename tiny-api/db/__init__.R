box::use(
  DBI[
    dbConnect,
    dbReadTable,
    dbWriteTable,
    dbDisconnect,
    dbExistsTable,
    dbRemoveTable,
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
#'
#' @return data.table. If a table with `name` is not found,
#'         returns a 0-row data.table.
#'
#' @export
read_table <- function(name) {
  con <- db_connect()
  on.exit(dbDisconnect(conn = con))

  if (!dbExistsTable(conn = con, name = name)) {
    return(
      data.table()
    )
  }

  dbReadTable(conn = con, name = name) |>
    as.data.table()
}
