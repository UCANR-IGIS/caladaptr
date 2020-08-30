#' Use a sf data frame as the location for a Cal-Adapt API request
#'
#' Specifies a sf data frame as the location for a Cal-Adapt API request
#'
#' @param x A Cal-Adapt API request
#' @param loc A sf data frame
#'
#' @details Not yet enabled.
#'
#' The sf data frame must have a valid CRS (doesn't have to be
#' geographic coords.)
#'
#' @export

ca_loc_sf <- function(x = ca_apireq(), loc) {

  if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq")
  if (!inherits(loc, "sf")) stop("loc should be a simple feature data frame")

  X$loc <- list(type="sf", val = loc)
  invisible(X)


  # ## THIS IS NOT NEEDED
  # if (nargs() == 1) {
  #
  #   ## One argument passed
  #   if (inherits(x, "sf")) {
  #     res <- caladaptr::ca_apireq()
  #     res$geom <- list(type="sf", val = x)
  #
  #   } else if (inherits(loc, "sf")) {
  #     res <- caladaptr::ca_apireq()
  #     res$geom <- list(type="sf", val = loc)
  #
  #   } else {
  #     stop("sf object expected")
  #
  #   }
  #
  #
  # } else {
  #   ## Two arguments passed
  #   if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq")
  #   if (!inherits(loc, "sf")) stop("loc should be a simple feature data frame")
  #   res <- x
  #   res$geom <- list(type="sf", val = loc)
  #
  # }

  #invisible(res)


}
