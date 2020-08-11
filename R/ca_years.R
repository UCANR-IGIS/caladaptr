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
  if (start < 1950) stop("Start year should be 1950 or later")
  if (end > 2100) stop("The largest value of the end year is 2100")

  start_dt <- as.Date(paste0(start, "-01-01"), optional = TRUE)
  end_dt <- as.Date(paste0(end, "-12-31"), optional = TRUE)

  if (is.na(start_dt) || is.na(end_dt)) stop("Unknown year format. Please enter year as a 4-digit number.")
  if (start_dt >= end_dt) stop("The end date should come after the start date")

  x$dates <- list(start = start_dt, end = end_dt)

  invisible(x)

}
