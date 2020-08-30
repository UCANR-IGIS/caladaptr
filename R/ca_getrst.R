#' Get raster from an API request object
#'
#' @param x A Cal-Adapt API request
#' @param quiet Suppress messages

#' @export

ca_getrst <- function(x, quiet = TRUE) {

  if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq")

  stop('ca_getrst is not yet supported')

  #if (is.na(x$resource)) stop("dataset has not been specified in the api call")

  #if (x$resource$type == "scripps_loca") {

    ## GET THE SLUG




    ## Create the geom part of the URL
    # g_wkt <- NULL
    #
    # if (is.numeric(loc)) {
    #   if (length(loc) == 2) {
    #     ## Probably a lat-lon coord
    #     g_wkt <-sf::st_as_text(sf::st_point(loc))
    #   } else {
    #     ## Check to see if its a zip code
    #   }
    # }
    # if (is.null(g_wkt)) error("Don't know how to handle that location")



  #} else {
    #stop("this dataset is not yet supported")
  #}

}
