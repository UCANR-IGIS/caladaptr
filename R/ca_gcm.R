#' Assign GCM(s) to a Cal-Adapt API call
#'
#' Specifies GCM(s) a Cal-Adapt API call should retrieve
#'
#' @param x Cal-Adapt API request
#' @param gcm Global Climate Model abbreviation
#'
#' @details
#' For valid options for \code{gcm}, see \code{\link{gcms}}.
#'
#' @export

ca_gcm <- function(x = ca_apireq(), gcm) {

  if (!inherits(x, "ca_apireq")) stop("x should be an object of class ca_apireq")

  if (FALSE %in% (gcm %in% gcms)) {
    stop(paste0("Unknown value(s) in gcm: ",
                paste0(gcm[!gcm %in% gcms], collapse = ", " ), ". Run `gcms` for valid values."))
  }

  x$gcm <- gcm
  invisible(x)

}

