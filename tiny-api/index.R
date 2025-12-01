box::use(
  ambiorix[Ambiorix],
  . /
    controllers /
    home[
      home_get,
    ],
)

app <- Ambiorix$new()
app$static("public", "static")
app$get("/", home_get)

app$start(port = 3000L)
