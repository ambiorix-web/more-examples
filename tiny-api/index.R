box::use(
  ambiorix[Ambiorix],
  . /
    controllers /
    home[
      home_get,
      home_post,
    ],
)

app <- Ambiorix$new()
app$static("public", "static")
app$get("/", home_get)
app$post("/", home_post)

app$start(port = 3000L)
