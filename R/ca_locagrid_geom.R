#' Get the geometry of the LOCA data set vector grid.
#'
#' Get the geometry of the LOCA data set vector grid.
#'
#' @param quiet Suppress messages
#'
#' @details This retrieves the geometry of the LOCA grid as a vector (polygon)
#' layer. The cells in this grid represent the pixels for all the LOCA downscaled
#' raster series on Cal-Adapt. A copy of the layer will be saved in the cache folder
#' so it won't have to be downloaded more than once.
#'
#' @return A simple feature data frame
#'
#' @seealso \code{\link{ca_getcache}}
#'
#' @importFrom sf st_read
#' @importFrom utils download.file unzip
#' @importFrom curl has_internet
#' @export

ca_locagrid_geom <- function(quiet = FALSE) {

  ## TODO Consider adding an argument to download in temp space (or a user defined directory),
  ## for people who can't or don't want to use a cache dir

  cache_dir <- ca_getcache()

  ## Construct the expected name to the geopackage
  gpkg_fn <- file.path(cache_dir, "locagrid.gpkg")

  if (!file.exists(gpkg_fn)) {
    ## Try to download it
    if (!has_internet()) stop("No internet connection")
    if (!quiet) {
      msg <- getOption("ca_message", paste0)
      message(msg("Downloading locagrid.zip from GitHub"))
    }
    gpkg_url <- "https://github.com/ucanr-igis/caladaptr-res/raw/main/geoms/locagrid.zip"
    tmp_zipfn <- tempfile(fileext = ".zip")
    download_success <- download.file(url = gpkg_url, destfile = tmp_zipfn,
                                      quiet = quiet)
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
