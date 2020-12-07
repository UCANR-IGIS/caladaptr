#' Get the geometry of an AOI Preset area
#'
#' Get the geometry of an AOI Preset area
#'
#' @param aoipreset The name of a AOI preset
#' @param quiet Suppress messages
#'
#' @details This retrieves the geometry (i.e., boundaries) for one of Cal-Adapt's AOI Presets.
#' If the spatial layer has not already been downloaded, it will be downloaded
#' (from \url{https://github.com/ucanr-igis/caladaptr/tree/master/aoipreset_geoms}) and saved in
#' the local cache directory as a GeoPackage. The default local cache directory is buried in
#' the current user's 'AppData' folder. To put the GeoPackages in an easier-to-find location,
#' use \code{\link{ca_setcache}}.
#'
#' @return A simple feature data frame
#'
#' @seealso \code{\link{aoipreset_types}}, \code{\link{ca_getcache}}
#'
#' @importFrom sf st_read
#' @importFrom utils download.file unzip
#' @importFrom curl has_internet
#' @export

ca_aoipreset_geom <- function(aoipreset, quiet = FALSE) {

  ## Consider adding an argument to download in temp space (or a user defined directory),
  ## for people who can't or don't want to use a cache dir

  if (length(aoipreset) > 1) stop("Sorry, aoipreset can only accept a single value")

  if (!aoipreset %in% aoipreset_types) {
    #stop(paste0("unknown preset(s): ", paste(aoipreset[!aoipreset %in% aoipreset_types], collapse=", ")))
    stop(paste0("unknown preset(s): ", aoipreset))
  }

  ## Consider making dir an argument, for people who can't or don't want to use a cache dir
  cache_dir <- ca_getcache()

  ## Construct the expected name to the geopackage
  gpkg_fn <- file.path(cache_dir, paste0(aoipreset, ".gpkg"))

  if (!file.exists(gpkg_fn)) {
    if (!has_internet()) stop("No internet connection")
    if (!quiet) {
      msg <- getOption("ca_message", paste0)
      message(msg(paste0("Downloading ", aoipreset, ".zip from GitHub")))
    }
    ## Try to download it
    gpkg_url <- paste0("https://github.com/ucanr-igis/caladaptr/raw/master/geoms/", aoipreset, ".zip")
    tmp_zipfn <- tempfile(aoipreset, fileext = ".zip")
    download_success <- download.file(url = gpkg_url, destfile = tmp_zipfn, quiet = quiet)
    if (download_success == 0) {
      unzip(tmp_zipfn, exdir = cache_dir)
      unlink(tmp_zipfn)
      if (!quiet) {
        success <- getOption("ca_success", paste0)
        message(success("Done"))
      }

    }
  }

  if (file.exists(gpkg_fn)) {
    st_read(gpkg_fn, quiet = quiet)
  } else {
    NA
  }

}
