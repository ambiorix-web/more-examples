box::use(
  ambiorix[Ambiorix],
  htmltools[
    tags,
    tagList,
  ],
  . /
    controllers /
    contact[
      contact_get,
      contact_post,
    ],
  . /
    controllers /
    home[
      home_get
    ],
)

app <- Ambiorix$new()

app$static(path = "public", uri = "/static")

app$get("/", home_get)
app$get("/contact", contact_get)
app$post("/contact", contact_post)

app$start(port = 8000L)
