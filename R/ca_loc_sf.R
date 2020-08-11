#' Specifies the location based on a sf object
#'
#' @export

ca_loc_sf <- function(x, loc) {

  if (nargs() == 1) {

    ## One argument passed
    if (inherits(x, "sf")) {
      res <- caladaptr::ca_apireq()
      res$geom <- list(type="sf", val = x)

    } else if (inherits(loc, "sf")) {
      res <- caladaptr::ca_apireq()
      res$geom <- list(type="sf", val = loc)

    } else {
      stop("sf object expected")

    }


  } else {
    ## Two arguments passed
    if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq")
    if (!inherits(loc, "sf")) stop("loc should be a simple feature data frame")
    res <- x
    res$geom <- list(type="sf", val = loc)

  }

  invisible(res)


}
