#' Format a ca_apireq object
#'
#' @param x Cal-Adapt API request
#' @param ... Unused
#' @import crayon
#' @importFrom sf st_geometry_type
#' @export
#' @method format ca_apireq

format.ca_apireq <- function(x, ...) {

  accent1 <- getOption("ca_accent1", paste0)
  accent2 <- getOption("ca_accent2", paste0)

  loc1 <- accent2("Location(s): ")

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
                             ifelse(length(x$loc$val$idval) > 3, paste0(", (+ ", length(x$loc$val$idval) - 3, " more)"), ""))
      }
      loc2 <- paste0("\n  AOI Preset: ", x$loc$val$type,
                     "\n  ", x$loc$val$idfld, "(s): ", idvals_str)

    } else if (x$loc$type == "zip") {

      loc2 <- paste0("\n  ", x$loc$val$type, "\n  ", x$loc$val$idfld, ": ",
                    paste(x$loc$val$idval, collapse=", "))

    } else if (x$loc$type == "pt") {

      if (nrow(x$loc$val) > 3) {
        str_dotdotdot <- paste0(", (+ ", nrow(x$loc$val) - 3, " more)")
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

  if (identical(x$cvar, NA)) {
    vars_str <- ""
  } else {
    vars_str <- paste0(accent2("Variable(s): "), paste(x$cvar, collapse = ", "), "\n")
  }

  if (identical(x$gcm, NA)) {
    gcm_str <- ""
  } else {
    gcm_str <- paste0(accent2("GCM(s): "), paste(x$gcm, collapse = ", "), "\n")
  }

  if (identical(x$scenario, NA)) {
    scen_str <- ""
  } else {
    scen_str <- paste0(accent2("Scenario(s): "), paste(x$scenario, collapse = ", "), "\n")
  }

  if (identical(x$period, NA)) {
    per_str <- ""
  } else {
    per_str <- paste0(accent2("Temporal aggregration period(s): "), paste(x$period, collapse = ", "), "\n")
  }

  if (identical(x$livneh, NA)) {
    livneh_str <- ""
  } else {
    livneh_str <- paste0(accent2("Livneh data: "), "True", "\n")
  }

  if (identical(x$slug, NA)) {
    slug_str <- ""
  } else {
    slug_str <- paste0(accent2("Slug(s): "), paste(x$slug, collapse = ", "), "\n")
  }

  if (identical(x$dates, NA)) {
    dates_str <- ""
  } else {
    dates_str <- paste0(accent2("Dates: "), x$dates$start, " to ", x$dates$end, "\n")
  }

  if (identical(x$options, NA)) {
    options_str <- ""
  } else {
    options_str <- paste0(accent2("Options:\n  spatial ag: "),
                          paste(x$options$spatial_ag, collapse = ", "),
                          "\n")
    # NOT IN USE:
    # "\n  temporal ag (add'l): ", x$options$temporal_ag,
  }

  invisible(paste0(loc_str, vars_str, per_str, livneh_str, gcm_str, scen_str, slug_str, dates_str, options_str))

}

#' Print a ca_apireq object
#'
#' @param x Cal-Adapt API request
#' @param ... Unused
#' @import crayon
#' @method print ca_apireq
#' @export

print.ca_apireq <- function(x, ...) {
  accent1 <- getOption("ca_accent1", paste0)
  cat(accent1("Cal-Adapt API Request\n"))
  cat(format(x), "\n")
}

#' Plot a ca_apireq object
#'
#' @param x Cal-Adapt API request
#' @param basemap The name of a basemap tile layer (see tm_basemap)
#' @param locagrid Overlay a portion of the LOCA downscaled grid
#' @param static Plot a static map instead of a interactive leaflet map
#' @param ... Unused
#' @import crayon
#' @importFrom tmap tm_shape tm_polygons tm_symbols tm_basemap tm_view tm_borders
#' @importFrom sf st_as_sf st_read st_geometry st_geometry_type st_transform st_buffer st_as_sfc st_bbox
#' @importFrom dplyr slice filter select
#' @method plot ca_apireq
#' @export

plot.ca_apireq <- function(x,
                           basemap = c("Esri.NatGeoWorldMap", "OpenStreetMap")[1],
                           locagrid = FALSE,
                           static = FALSE, ...) {

  if (static) {
    options(tmap.mode = "plot")
  } else {
    options(tmap.mode = "view")
  }

  if (locagrid) locagrid_sf <- ca_locagrid_geom(quiet = TRUE) %>% st_transform(3857)

  if (x$loc$type == "pt") {

    # When type = "pt", val = a 3-column data frame with columns id, x, y
    pts_sf <- st_as_sf(x$loc$val, coords = c("x", "y"), crs = 4326) %>%
      st_transform(3857)

    api_map <- tm_shape(pts_sf) +
      tm_basemap(basemap) +
      tm_symbols(col = "red", alpha = 0.8, size = 0.2, group = "Query Location(s)") +
      tm_view(symbol.size.fixed = TRUE)

    if (locagrid) {
      ## Get the bounding box of the features, buffer by 10km
      bbox_sf <- st_as_sfc(st_bbox(pts_sf)) %>% st_buffer(dist = 15000)
      api_map <- api_map +
        tm_shape(locagrid_sf[bbox_sf, ]) +
        tm_borders(col="dimgray", group = "LOCA grid")
    }

    api_map

  } else if (x$loc$type == "aoipreset") {

    ## Get the vector layer
    preset_name <- x$loc$val$type
    polyall_sf <- ca_aoipreset_geom(preset_name, quiet = TRUE) %>% st_transform(3857)

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

    ## Generate the plot if a feature was found
    if (!is.null(polyuse_sf)) {

      api_map <- tm_shape(polyuse_sf) +
        tm_basemap(basemap) +
        tm_polygons(col = "red", alpha = 0.2, group = "Query Location(s)")

      if (locagrid) {
        ## Get the bounding box of the features, buffer by 10km
        bbox_sf <- st_as_sfc(st_bbox(polyuse_sf)) %>% st_buffer(dist = 10000)
        api_map <- api_map +
          tm_shape(locagrid_sf[bbox_sf, ]) +
          tm_borders(col="dimgray", group = "LOCA grid")
      }

      api_map

    }


  } else if (x$loc$type == "sf") {

    loc_webmerc <- x$loc$val$loc %>% st_transform(3857)

    if (unique(as.character(st_geometry_type(loc_webmerc))) %in% c("POLYGON", "MULTIPOLYGON")) {

      ## Polygon layer
      api_map <- tm_shape(loc_webmerc) +
        tm_basemap(basemap) +
        tm_polygons(col = "red", alpha = 0.2, group = "Query Location(s)")

    } else {   ## point layer
      api_map <- tm_shape(loc_webmerc) +
        tm_basemap(basemap) +
        tm_symbols(col = "red", alpha = 0.8, size = 0.2, group = "Query Location(s)") +
        tm_view(symbol.size.fixed = TRUE)
    }


    if (locagrid) {
      ## Add the locagrid to the map
      bbox_sf <- st_as_sfc(st_bbox(loc_webmerc)) %>% st_buffer(dist = 10000)
      api_map <- api_map +
        tm_shape(locagrid_sf[bbox_sf, ]) +
        tm_borders(col="dimgray", group = "LOCA grid")
    }

    api_map

  } else if (x$loc$type == "zip") {
    msg <- getOption("ca_message", paste0)
    message(msg("Sorry, plotting this location type is not yet supported"))

  }

}


