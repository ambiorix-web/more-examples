#' Format number with thousand separators
#'
#' @param x Numeric value.
#' @param digits Number of decimal places.
#'
#' @return Formatted string.
#' @export
format_number <- function(x, digits = 0L) {
  rounded <- round(x, digits = digits)

  format(
    rounded,
    big.mark = ",",
    trim = TRUE,
    scientific = FALSE,
    nsmall = digits
  )
}

#' Format value as percentage
#'
#' @param x Numeric value (0-1 scale).
#' @param digits Number of decimal places.
#'
#' @return Formatted percentage string.
#' @export
format_percent <- function(x, digits = 1L) {
  sprintf(paste0("%.", digits, "f%%"), x * 100)
}

#' Calculate trend info
#'
#' @param current Current value.
#' @param baseline Baseline value for comparison.
#' @param label Label for the trend.
#' @param direction Whether up is good or down is good.
#'
#' @return List with text and status, or NULL if baseline is invalid.
#' @export
trend_info <- function(
  current,
  baseline,
  label,
  direction = c("up-is-good", "down-is-good")
) {
  direction <- match.arg(arg = direction)

  if (is.null(baseline) || is.na(baseline) || identical(baseline, 0)) {
    return(NULL)
  }

  change <- (current - baseline) / baseline
  status <- if (direction == "up-is-good") {
    if (change >= 0) "up" else "down"
  } else {
    if (change <= 0) "up" else "down"
  }

  list(
    text = sprintf("%+.1f%% %s", change * 100, label),
    status = status
  )
}
