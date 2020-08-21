#' Add processing options to a Cal-Adapt API call
#'
#' Specify processing options for a Cal-Adapt API call
#'
#' @param x Cal-Adapt API request
#' @param spatial_ag Spatial aggregation function for polygon locs.
#' @param temporal_ag List object specifying unit of time and summary function(s) for temporal aggregation. NOT YET SUPPORTED
#'
#' @details
#' \code{spatial_ag} is the name(s) of summary statistic(s) that will be used when querying
#' polygon locations to aggregate the values of pixel that fall within the
#' area of interest. Values can be \code{mean}, \code{max}, \code{median}, \code{min},
#' and \code{sum}. To get retrieve the individual values for all pixels without a summary, use
#' the \code{ca_getrst} function.
#'
#' When querying point locations (e.g., \code{\link{ca_loc_pt} or \code{ca_loc_zip}, \code{spatial_ag} should be set to
#' \code{'none'}.
#'
#' \code{temporal_ag} allows you to apply an \emph{additional} temporal
#' aggregation function, on top of any temporal aggregation the data are already summarized by
#' (e.g., month or year). When querying climate layers that are already temporally aggregated,
#' the unit of temporal aggregation must be a larger unit of time (e.g., you can't pull down annual average
#' layers and then try to aggregate them by month).
#'
#' @export

ca_options <- function(x = ca_apireq(),
                       spatial_ag = c("none", "mean", "max", "median", "min", "sum")[1],
                       temporal_ag = NA) {

  if (!inherits(x, "ca_apireq")) stop("x should be an object of class ca_apireq")

  if (is.null(spatial_ag)) stop("spatial_ag is a required argument")
  if (is.null(temporal_ag)) stop("temporal_ag is a required argument")

  valid_spatial_ag <- c("none", "mean", "max", "median", "min", "sum")
  if (FALSE %in% (spatial_ag %in% valid_spatial_ag)) {
    stop(paste0("Unknown value(s) for spatial_ag: ",
                paste(spatial_ag[!spatial_ag %in% valid_spatial_ag], collapse=", ")))
  }

  x$options <- list(spatial_ag = spatial_ag, temporal_ag = temporal_ag)
  invisible(x)

}

