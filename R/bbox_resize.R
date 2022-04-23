#' Resize a bounding box object
#'
#' Resize a bounding box object by a scale factor or buffer distance
#'
#' @param x bbox object
#' @param scale scale factor
#' @param buff buffer distance
#'
#' @details
#' You can resize a bounding box by passing a value for \code{scale} or \code{buff}, but not both.
#'
#' Use \code{scale} to resize the bounding box with a scale factor. Values of \code{scale} < 1 will
#' result in a smaller bounding box, values > 1 will result in a larger bounding box. The centroid will
#' remain the same. If you pass two values for \code{scale}, they'll be used to scale the x and y
#' dimensions respectively
#'
#' Use \code{buff} to resize the bounding box with a fixed distance. \code{buff} should be in a map units.
#' Values of \code{buff} < 0 will result in a smaller bounding box. Values > 0 will result in a larger
#' bounding box. If you pass two values for \code{buff}, they'll be used to buffer the x and y
#' dimensions respectively
#'
#' @export

bbox_resize <- function(x, scale = NULL, buff = NULL) {

  if (!inherits(x, "bbox")) stop("x should be an object of class bbox")
  if (is.null(scale) && is.null(buff)) stop("You must pass scale or buff")
  if (!is.null(scale) && !is.null(buff)) stop("Please pass scale or buff (not both")

  if (!is.null(scale)) {
    if (length(scale) == 1) scale <- rep(scale, 2)
    ## Get the x-range and y-range of the output bbox
    xrange <- diff(x[c("xmin", "xmax")]) * scale[1]
    yrange <- diff(x[c("ymin", "ymax")]) * scale[2]

  } else if (!is.null(buff)) {
    if (length(buff) == 1) buff <- rep(buff, 2)
    xrange <- diff(x[c("xmin", "xmax")]) + (buff[1] * 2)
    yrange <- diff(x[c("ymin", "ymax")]) + (buff[2] * 2)
  }

  x_ctr <- mean(x[c("xmin", "xmax")])
  y_ctr <- mean(x[c("ymin", "ymax")])

  ## Make a copy of the bbox (that we'll modify and return)
  bbox_new <- x
  bbox_new[c("xmin", "xmax")] <- x_ctr + (xrange / 2) * c(-1,1)
  bbox_new[c("ymin", "ymax")] <- y_ctr + (yrange / 2) * c(-1,1)

  ## Return the next bbox
  bbox_new

}

