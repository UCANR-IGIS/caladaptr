#' Run checks on an API request object
#'
#' Run checks on an API request object
#'
#' @param x A Cal-Adapt API request
#' @param slug_check Cross check the slug against the raster series catalog
#' @param date_check Cross check the start and end date against the raster series catalog
#' @param loc_check Check to make sure the location is within the Cal-Adapt coverage area
#' @param units_check Check for consistent units
#' @param ignore_spag Ignore the spatial aggregation option
#' @param quiet Suppress messages
#'
#' @details This function checks an API request for errors.
#'
#' If a slug is not found, the variable may not be available for the temporal aggregation period.
#' See \code{\link{ca_catalog_rs}}.
#'
#' @export

ca_preflight <- function(x, slug_check = TRUE, date_check = TRUE, loc_check = TRUE,
                         units_check = TRUE, ignore_spag = FALSE, quiet = FALSE) {

  if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq")

  res <- ca_apicalls(x,
                     slug_check = slug_check,
                     date_check = date_check,
                     loc_check = loc_check,
                     units_check = units_check,
                     ignore_spag = ignore_spag,
                     preflight = TRUE)

  if (!quiet) {
    accent2 <- getOption("ca_accent2", paste0)
    message(accent2("API request ok"))
  }

  invisible(res)

}



