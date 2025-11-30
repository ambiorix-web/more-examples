box::use(
  stats[
    rexp,
    runif,
    rnorm,
    rlnorm,
  ],
  grDevices[
    png,
    dev.off,
  ],
  utils[
    capture.output,
  ],
  graphics[hist],
  base64enc[base64encode],
)

#' @export
random_distribution_labels <- c(
  norm = "Normal",
  unif = "Uniform",
  lnorm = "Log-normal",
  exp = "Exponential"
)

#' Sanitize distribution name
#'
#' @param dist Distribution name.
#'
#' @return Valid distribution name.
#' @export
sanitize_dist <- function(dist) {
  if (is.null(dist) || !dist %in% names(random_distribution_labels)) {
    return("norm")
  }
  dist
}

#' Sanitize sample size
#'
#' @param n Sample size.
#'
#' @return Valid sample size between 1 and 1000.
#' @export
sanitize_n <- function(n) {
  if (is.null(n)) {
    n <- 500L
  }

  n <- as.integer(n)

  if (is.na(n)) {
    n <- 500L
  }

  n <- max(1L, min(1000L, n))
  n
}

#' Generate histogram as base64 encoded PNG
#'
#' @param values Numeric vector of values.
#' @param dist Distribution name.
#' @param n Sample size.
#'
#' @return Base64 encoded PNG data URI.
#' @export
generate_histogram_src <- function(values, dist, n) {
  path <- tempfile(fileext = "png")
  on.exit(unlink(x = path))

  png(filename = path, width = 800, height = 500)
  hist(
    values,
    main = sprintf("r%s(%s)", dist, n),
    col = "#75AADB",
    border = "white"
  )
  dev.off()

  binary <- readBin(
    con = path,
    what = "raw",
    n = file.info(path)$size
  )

  paste0("data:image/png;base64,", base64encode(binary))
}

#' Generate random demo data
#'
#' @param params List with dist and n parameters.
#'
#' @return List with distribution info, histogram, summary, and table.
#' @export
generate_random_demo <- function(params) {
  dist <- sanitize_dist(params$dist)
  n <- sanitize_n(params$n)

  generator <- switch(
    dist,
    norm = rnorm,
    unif = runif,
    lnorm = rlnorm,
    exp = rexp
  )

  values <- generator(n)
  histogram <- generate_histogram_src(values, dist, n)
  summary_lines <- capture.output(summary(values))

  display_n <- min(length(values), 20L)
  table_values <- data.frame(
    Index = seq_len(display_n),
    Value = format(round(values[seq_len(display_n)], 4), nsmall = 4)
  )

  list(
    dist = dist,
    dist_label = random_distribution_labels[[dist]],
    n = n,
    histogram = histogram,
    summary = summary_lines,
    table = table_values,
    query = list(dist = dist, n = n)
  )
}
