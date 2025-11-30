box::use(
  ambiorix[
    Ambiorix,
    Router,
  ],
  . /
    controllers /
    pages[
      root_get,
      pages_get,
      page_get,
      panel_get,
      tab_get,
    ],
)

app <- Ambiorix$new()

app$static(path = "public", uri = "/static")

app$get("/", root_get)

pages_router <- Router$new("/pages")
pages_router$get("/", pages_get)
pages_router$get("/:page", page_get)
pages_router$get("/:page/:panel", panel_get)
pages_router$get("/:page/:panel/:tab", tab_get)

app$use(pages_router)

app$start(port = 3000L)
