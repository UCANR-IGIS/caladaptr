#' Use Livneh data in a Cal-Adapt API call
#'
#' Specify Livneh data should be retrieved in a Cal-Adapt API call
#'
#' @param x Cal-Adapt API request
#' @param livneh Use Livneh data, logical
#'
#' @details
#'
#' @seealso \code{\link{ca_catalog_rs}}
#'
#' @export

ca_livneh <- function(x = ca_apireq(), livneh = TRUE) {

  if (!inherits(x, "ca_apireq")) stop("x should be an object of class ca_apireq")

  ## Error check
  if (!is.logical(livneh)) {
    stop("Argument livneh should be logical")
  }

  x$livneh <- livneh
  invisible(x)

}

