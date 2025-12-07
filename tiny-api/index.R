box::use(
  ambiorix[Ambiorix],
  . /
    controllers /
    home[
      home_get,
      home_post,
      table_get,
      table_delete,
      tables_get,
      docs_get,
    ],
)

app <- Ambiorix$new()
app$static("public", "static")
app$get("/", home_get)
app$post("/", home_post)
app$get("/docs", docs_get)
app$get("/tables", tables_get)
app$get("/:table", table_get)
app$delete("/:table", table_delete)

app$start(port = 3000L)
