box::use(
  .. /
    ui /
    home[
      UI,
    ],
)

home_get <- function(req, res) {
  res$send(UI())
}
