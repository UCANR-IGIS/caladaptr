#' Format a ca_apireq object
#'
#' @param x Cal-Adapt API request
#' @param ... Unused
#' @importFrom crayon yellow bold cyan
#' @importFrom sf st_geometry_type
#' @export
#' @method format ca_apireq

format.ca_apireq <- function(x, ...) {

  colorme <- cyan

  loc1 <- colorme("Location(s): ")

  if (identical(x$loc, NA)) {
    loc2 <- "NA"

  } else if (is.list(x$loc)) {

    if (x$loc$type == "sf") {

      loc2 <- paste0("\n  Simple Feature ", unique(st_geometry_type(x$loc$val$loc)),
                     " (", nrow(x$loc$val$loc), " feature(s))",
                     "\n  ID field: ", x$loc$val$idfld)

      if (x$loc$val$dTolerance > 0) {
        loc2 <- paste0(loc2,
                       "\n  dTolerance: ", x$loc$val$dTolerance)
      }


    } else if (x$loc$type == "aoipreset") {

      ## Construct the list of idvals
      if (is.null(x$loc$val$idval)) {
        idvals_str <- "all"
      } else {
        idvals_str <- paste0(paste(head(x$loc$val$idval, n=3), collapse=", "),
                             ifelse(length(x$loc$val$idval) > 3, ", ...", ""))
      }
      loc2 <- paste0("\n  AOI Preset: ", x$loc$val$type,
                     "\n  ", x$loc$val$idfld, "(s): ", idvals_str)

    } else if (x$loc$type == "zip") {

      loc2 <- paste0("\n  ", x$loc$val$type, "\n  ", x$loc$val$idfld, ": ",
                    paste(x$loc$val$idval, collapse=", "))

    } else if (x$loc$type == "pt") {

      if (nrow(x$loc$val) > 3) {
        str_dotdotdot <- ", ..."
      } else {
        str_dotdotdot <- ""
      }

      pts3 <- round(as.matrix(head(x$loc$val[, c(2,3)], n=3)), 3)
      loc2 <- paste0("\n  x: ", paste(pts3[,1], collapse=", "), str_dotdotdot,
                     "\n  y: ", paste(pts3[,2], collapse=", "), str_dotdotdot)

    } else {
      loc2 <- "unknown location type"
    }
  }
  loc_str <- paste0(loc1, loc2, "\n")

  vars_str <- paste0(colorme("Variable(s): "), paste(x$cvar, collapse = ", "), "\n")

  gcm_str <- paste0(colorme("GCM(s): "), paste(x$gcm, collapse = ", "), "\n")

  scen_str <- paste0(colorme("Scenario(s): "), paste(x$scenario, collapse = ", "), "\n")

  per_str <- paste0(colorme("Temporal aggregration period(s): "), paste(x$period, collapse = ", "), "\n")

  slug_str <- paste0(colorme("Slug(s): "), paste(x$slug, collapse = ", "), "\n")

  if (identical(x$dates, NA)) {
    dates_val <- "NA"
  } else {
    dates_val <- paste(x$dates$start, "to", x$dates$end)
  }
  dates_str <- paste0(colorme("Dates: "), dates_val, "\n")

  if (identical(x$options, NA)) {
    opt_obj <- "NA"
  } else {
    opt_obj <- paste0("\n  spatial ag: ", paste(x$options$spatial_ag, collapse = ", "),
                      "\n  temporal ag (add'l): ", x$options$temporal_ag)
  }
  options_str <- paste0(colorme("Options: "), opt_obj, "\n")

  invisible(paste0(loc_str, vars_str, per_str, gcm_str, scen_str, slug_str, dates_str, options_str))

}

#' Print a ca_apireq object
#'
#' @param x Cal-Adapt API request
#' @param ... Unused
#' @importFrom crayon yellow bold cyan
#' @method print ca_apireq
#' @export

print.ca_apireq <- function(x, ...) {
  colorme <- cyan
  cat(colorme$bold("Cal-Adapt API Request\n"))
  cat(format(x), "\n")
}

#' Plot a ca_apireq object
#'
#' @param x Cal-Adapt API request
#' @param basemap The name of a basemap tile layer (see tm_basemap)
#' @param static Plot a static map instead of a interactive leaflet map
#' @param ... Unused
#' @importFrom crayon yellow bold
#' @importFrom tmap tm_shape tm_polygons tm_symbols tm_basemap
#' @importFrom sf st_as_sf st_read st_geometry st_geometry_type
#' @importFrom dplyr slice filter select
#' @method plot ca_apireq
#' @export

plot.ca_apireq <- function(x,
                           basemap = c("Esri.NatGeoWorldMap", "OpenStreetMap")[1],
                           static = FALSE, ...) {

  if (static) {
    options(tmap.mode = "plot")
  } else {
    options(tmap.mode = "view")
  }

  if (x$loc$type == "pt") {

    # When type = "pt", val = a 3-column data frame with columns id, x, y
    pts_sf <- st_as_sf(x$loc$val, coords = c("x", "y"), crs = 4326)
    tm_shape(pts_sf) +
      tm_basemap(basemap) +
      tm_symbols(col = "red", alpha = 0.8, scale = 1.5)

  } else if (x$loc$type == "aoipreset") {

    ## Get the vector layer
    preset_name <- x$loc$val$type
    polyall_sf <- ca_aoipreset_geom(preset_name, quiet = TRUE)

    if (is.null(x$loc$val$idval)) {
      polyuse_sf <- polyall_sf

    } else {

      ## Find the feature(s)
      feat_idx <- which(polyall_sf[[x$loc$val$idfld]] %in% x$loc$val$idval)

      if (length(feat_idx) == 0) {
        warning("Feature(s) not found")
        polyuse_sf <- NULL

      } else {
        polyuse_sf <- polyall_sf %>% slice(feat_idx)
      }
    }

    ## Generate the plot
    if (!is.null(polyuse_sf)) {
      tm_shape(polyuse_sf) +
        tm_basemap(basemap) +
        tm_polygons(col = "red", alpha = 0.2)
    }


  } else if (x$loc$type == "sf") {

    if (unique(as.character(st_geometry_type(x$loc$val$loc))) %in% c("POLYGON", "MULTIPOLYGON")) {

      tm_shape(x$loc$val$loc) +
        tm_basemap(basemap) +
        tm_polygons(col = "red", alpha = 0.2)

    } else {

      tm_shape(pts_sf) +
        tm_basemap(basemap) +
        tm_symbols(col = "red", alpha = 0.8, scale = 1.5)

    }



  } else if (x$loc$type == "zip") {
    message(yellow("Sorry, plotting this location type is not yet supported"))

  }

}


