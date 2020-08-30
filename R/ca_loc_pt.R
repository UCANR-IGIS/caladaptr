#' Assign point location to a Cal-Adapt API request
#'
#' Specifies point location(s) a Cal-Adapt API call should retrieve
#'
#' @param x A Cal-Adapt API request
#' @param coords A two-column matrix or data frame
#' @param id Unique id values for each point
#'
#' @details
#' \code{coords} should be a two-column matrix or data frame with x (longitude)
#' and y (latitude) coordinates for the point(s) of interest.
#'
#' \code{id} should be vector that uniquely identify the points. If omitted, row numbers will be used.
#'
#' @seealso \code{\link{ca_apireq}}
#' @export

ca_loc_pt <- function(x = ca_apireq(), coords, id=NULL) {

  if (!inherits(x, "ca_apireq")) stop("x should be an object of class ca_apireq")

  ## If pt is a numeric vector of length 2, convert it to a data frame
  if (is.numeric(coords) && length(coords) == 2) coords <- data.frame(x=coords[1], y = coords[2])

  if (!inherits(coords, "data.frame") && !inherits(coords, "matrix")) {
    stop("coords must be a two-column data frame or matrix with x and y values in geographic coordinates")
  }

  ## Add more error checking here including the range of x and y

  if (is.null(id)) {
    id <- 1:nrow(coords)
  } else {
    if (anyDuplicated(id) != 0) stop("id must contain a unique value for each point")
  }

  #res <- x
  X$loc <- list(type="pt", val = data.frame(id = id, x = coords[,1], y = coords[,2]))
  invisible(X)

}
