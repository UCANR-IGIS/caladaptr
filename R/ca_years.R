#' Adds the start and end year to a Cal-Adapt API call
#'
#' Specifies the start and end year of a Cal-Adapt API call
#'
#' @param x Cal-Adapt API request
#' @param start start year
#' @param end end year
#'
#' @export

ca_years <- function(x = ca_apireq(), start, end) {

  if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq")

  if (length(start) != 1 || length(end) != 1) stop("start and end should each be of length 1")

  err_msg_format <- "Unknown year format. Please enter year as a 4-digit number."

  if (is.na(start)) {
    start_dt <- NA
  } else {
    if (start < 1950) stop("Start year should be 1950 or later")
    start_dt <- as.Date(paste0(start, "-01-01"), optional = TRUE)
    if (is.na(start_dt)) stop(err_msg_format)
  }

  if (is.na(end)) {
    end_dt <- NA
  } else {
    if (end > 2100) stop("The largest value of the end year is 2100")
    end_dt <- as.Date(paste0(end, "-01-01"), optional = TRUE)
    if (is.na(end_dt)) stop(err_msg_format)
  }

  if (!is.na(start_dt) && !is.na(end_dt)) {
    if (start_dt >= end_dt) stop("The end date should come after the start date")
    x$dates <- list(start = start_dt, end = end_dt)
  } else {
    x$dates <- NA
  }

  invisible(x)

}
