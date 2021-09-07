#' Get the Cal-Adapt raster series data catalog
#'
#' Get a local copy of the Cal-Adapt raster series data catalog
#'
#' @param quiet Suppress messages
#'
#' @details
#' This retrieves a local copy of the Cal-Adapt 'catalog' of raster series available through the Cal-Adapt API.
#' A copy of the catalog comes with caladaptR. You can also fetch a new copy using \link{ca_catalog_fetch}.
#'
#' @return A tibble with columns of information about the raster series available through the Cal-Adapt API.
#'
#' @seealso \code{\link{ca_catalog_fetch}}, \code{\link{ca_getcache}}
#'
#' @importFrom utils read.csv
#'
#' @export

ca_catalog_rs <- function(quiet = FALSE) {

  msg_fmt <- getOption("ca_message", I)

  ## Define some constants
  rs_csv_fn <- "ca_catalog_rs.csv"

  ## We look 1) in the cache dir (if cache=T), and if we don't find anything there we get
  ## the one bundled with the package.

  res <- NULL

  cache_dir <- ca_getcache(quiet = TRUE)

  if (is.na(cache_dir)) {
    warning("Can't locate the cache directory")

  } else {
    rs_csv_pathfn <- file.path(cache_dir, rs_csv_fn)
    if (file.exists(rs_csv_pathfn)) {
      if (!quiet) message(msg_fmt(" - using raster series catalog from cache"))
      res <- read.csv(file = rs_csv_pathfn, stringsAsFactors = FALSE)
    }

  }

  ## If that didn't work, use the one that comes with the package
  if (is.null(res)) {
    if (!quiet) message(msg_fmt(" - using the raster series catalog bundled with caladaptr"))
    res <- read.csv(system.file("extdata", rs_csv_fn, package = "caladaptr"),
                    stringsAsFactors = FALSE)
  }

  ## Return the result
  res

}
