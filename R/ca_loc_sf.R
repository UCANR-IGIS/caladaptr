#' Use a sf data frame as the location for a Cal-Adapt API request
#'
#' Specifies a sf data frame as the location for a Cal-Adapt API request
#'
#' @param x A Cal-Adapt API request
#' @param loc A simple feature data frame
#' @param idfld The name of a column in loc containing unique values, or the name
#' @param idval A vector of unique values
#' @param dTolerance A numeric value used to simplify polgyons, see Details.
#'
#' @details \code{loc} should be a simple feature data frame with point or polygon features.
#' The sf object should have a valid CRS, but does not have to be geographic.
#' Both 'single' and 'multipart' polygons can be used, but only simple point features are
#' supported. To convert a multipoint feature layer into a single point layer, use \code{\link[sf]{st_cast}}.
#'
#' \code{idfld} should be the name of a column in \code{loc} containing unique values. When you
#' fetch values from Cal-Adapt, this column will be returned in the results to help you
#' join the values to other tables. Alternately, you can use \code{idfld} to pass the name of
#' a new column, together with a vector of unique values in \code{idval} (one for each row
#' in \code{loc}).
#'
#' Note you can not use \code{idval} as a filter. If you want to filter the features of \code{loc} to
#' query, use a filter expression as part of the value of \code{loc} (e.g., with
#' \code{\link[dplyr]{filter}} or \code{\link[dplyr]{slice}}).
#'
#' If \code{loc} is a polygon layer, you'll also need to specify how to spatially aggregate the queried
#' values if a feature overlaps more than one pixel. See \code{\link{ca_options}}.
#'
#' \code{dTolerance} is a value in decimal degrees used to simplify polygons. If \code{dTolerance > 0},
#' \code{\link[sf]{geos_unary}{st_simplify}} will be called to remove polygon nodes within \code{dTolerance} of another node,
#' before fetching data. This can reduce the amount of spatial data that needs to be sent to the server,
#' which can improve performance particularly when you have very fine grained polygons.
#' Simplifying polygons can modify (generally reduce) the pixels that a polygon overlaps, so use with caution.
#' \code{dTolerance =	0.001} represents <100m on the ground within Cal-Adapt range of latitude.
#'
#' @importFrom sf st_crs st_geometry_type
#' @export

ca_loc_sf <- function(x = ca_apireq(), loc, idfld = NULL, idval = NULL, dTolerance = 0) {

  if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq")
  if (!inherits(loc, "sf")) stop("loc should be a simple feature data frame")

  if (is.na(st_crs(loc))) stop("loc must have a CRS set")

  ## Verify that loc is polygon or point (not multipolyon)
  if (FALSE %in% (as.character(st_geometry_type(loc)) %in% c("POLYGON", "POINT", "MULTIPOLYGON"))) {
    stop(paste0("Unsupported geometry type: ",
                paste(unique(as.character(st_geometry_type(loc))), collapse = ", "),
                ". See help for details."))
    # warning(paste0("loc is not a simple feature of type POLYGON or POINT.\nYou passed ",
    #             paste(unique(as.character(st_geometry_type(loc))), collapse = ", "), "."))
  }

  if (is.null(idfld)) {
    idfld <- "id"
    if (!is.null(idval)) stop("If idval is passed, you must also pass a value for idfld")
    idval_use <- 1:nrow(loc)

  } else {

    if (idfld %in% names(loc)) {

      if (!is.null(idval)) stop(paste0(idfld, " is a column in loc. Therefore you should not pass idval"))

      idval_use <- loc[[idfld ]]
      if (anyDuplicated(idval_use) != 0) stop(paste0("`", idfld, "`` has duplicate values. Please use a column with unique values."))

    } else {

      ## idfld is not a column in loc. Turn our attention to idval.
      if (is.null(idval)) {

        ## To reduce user errors whereby idfld is the name of a column in loc that has been misspelled,
        ## we require idval to be passed if idfld is not in in the attribute table

        stop(paste0("`", idfld, "` is not a column in loc, hence you must also pass `idval`."))


      } else {
        err_msg <- "If `idfld` is not the name of a column in `loc`, then `idval` should be a vector of unique values equal in length to the number of features in `loc`."
        if (length(idval) != nrow(loc)) stop(err_msg)
        if (anyDuplicated(idval) != 0) stop(err_msg)
        idval_use <- idval

      }

    }


  }

  x$loc <- list(type="sf", val = list(loc = loc, idfld = idfld, idval = idval_use,
                                      dTolerance = dTolerance))
  invisible(x)

}
