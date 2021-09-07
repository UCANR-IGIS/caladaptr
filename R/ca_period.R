#' Assign temporal aggregation period to a Cal-Adapt API call
#'
#' Add a temporal aggregation period to a Cal-Adapt API request
#'
#' @param x Cal-Adapt API request
#' @param period Period of temporal aggregation
#'
#' @details
#' For valid options for \code{period}, run \code{\link{periods}}.
#'
#' Notes:
#'
#' @export

ca_period <- function(x = ca_apireq(), period) {

  if (!inherits(x, "ca_apireq")) stop("x should be an object of class ca_apireq")

  if (!identical(period, NA)) {
    if (FALSE %in% (period %in% periods)) {
      stop(paste0("Unknown value(s) in period: ",
                  paste0(period[!period %in% periods], collapse = ", " ), ". Run `periods` for valid values."))
    }
  }

  x$period <- period
  invisible(x)

}


