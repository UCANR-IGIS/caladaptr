#' Adds a start and end date of a Cal-Adapt API call
#'
#' Specifies the start and end date of a Cal-Adapt API call
#'
#' @param x Cal-Adapt API request
#' @param start start date entered as a character \emph{yyyy-mm-dd}
#' @param end end date entered as a character \emph{yyyy-mm-dd}
#'
#' @export

ca_dates <- function(x = ca_apireq(), start, end) {

  if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq")

  if (length(start) != 1 || length(end) != 1) stop("start and end should each be of length 1")

  err_msg_format <- "Unknown date format. Please enter dates as yyyy-mm-dd."

  if (is.na(start)) {
    start_dt <- NA
  } else {
    start_dt <- as.Date(start, optional = TRUE)
    if (is.na(start_dt)) stop(err_msg_format)
  }

  if (is.na(end)) {
    end_dt <- NA
  } else {
    end_dt <- as.Date(end, optional = TRUE)
    if (is.na(end_dt)) stop(err_msg_format)
  }

  ## if (is.na(start_dt) || is.na(end_dt)) stop("Unknown date format. Please enter dates as yyyy-mm-dd.")
  if (!is.na(start_dt) && !is.na(end_dt)) {
    if (start_dt >= end_dt) stop("The end date should come after the start date")
    x$dates <- list(start = start_dt, end = end_dt)
  } else {
    x$dates <- NA
  }

  invisible(x)

}
