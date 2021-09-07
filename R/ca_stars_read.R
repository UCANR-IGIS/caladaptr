#' Read downloaded TIF files into R
#'
#' @description
#' `r lifecycle::badge(deprecated)`
#'
#' `ca_read_stars()` was renamed to `ca_stars_read()` for a more consistent API.
#'
#' @seealso \code{\link{ca_stars_read}}
#' @keywords internal
#' @export

ca_read_stars <- function(x, read_sidecar = TRUE, proxy = FALSE) {
  lifecycle::deprecate_warn("0.6.0", "ca_read_stars()", "ca_stars_read()")
  ca_stars_read(x, sidecar = read_sidecar, proxy = proxy)
}

#' Read tif files from disk
#'
#' @param x File name(s) with path of tif files
#' @param sidecar Read sidecar files if they exist, logical
#' @param proxy Import the TIF file as a stars proxy
#'
#' @details
#' This function can be used to read tif files that were downloaded by \code{\link{ca_getrst_stars}}. It
#' is a lightweight wrapper around stars::read_stars(), with the ability to read the sidecar
#' files created by \code{\link{ca_getrst_stars}}. These sidecar files restore all of the attributes of
#' the array dimensions that are not otherwise preserved by the tif format.
#'
#' \code{proxy} signifies whether the tif file(s) should be read as stars proxy (e.g., pointer).
#' This is recommended if the rasters are potentially too large to fit into memory. With stars proxy
#' objects, rasters will be read into memory only when needed, at the cost of potentially slightly
#' slower performance. For details see
#' \href{https://r-spatial.github.io/stars/articles/stars2.html#stars-proxy-objects}{stars documentation}.
#'
#' @return A list of stars objects
#'
#' @seealso \code{\link{ca_getrst_stars}}
#'
#' @importFrom stars read_stars

#' @export

ca_stars_read <- function(x, sidecar = TRUE, proxy = FALSE) {

  if (!is.character(x)) stop("x must be a character vector of tif file names")

  ## First loop through x and make sure of the tiffs exist
  for (tiff_fn in x) {
    if (!file.exists(tiff_fn)) stop(paste0("Can't find: ", tiff_fn) )
  }

  ## Initialize the result as a blank list
  res <- list()

  singe_row_col_yn <- FALSE

  for (tiff_fn in x) {

    ## Read in this tif
    this_tiff_stars <- read_stars(tiff_fn, proxy = proxy)

    ## Bring in the side car with the attributes
    if (sidecar) {
      sidecar_fn <- gsub(".tif$", ".attr.rds", tiff_fn)
      if (file.exists(sidecar_fn)) {
        attr_lst <- readRDS(sidecar_fn)
        for (one_attr in names(attr_lst)) {
          attr(this_tiff_stars, one_attr) <- attr_lst[[one_attr]]
        }
      }
    }

    if (1 %in% dim(this_tiff_stars)[1:2]) {
      singe_row_col_yn <- TRUE
    }

    ## Save this stars object to the list we're going to return
    res[[gsub(".tif$", "", basename(tiff_fn))]] <- this_tiff_stars

  } ## loop through x

  # If needed show the single column / row warning
  if (singe_row_col_yn) {
    msg <- "This stars object has a single row or column of pixels. Tidyverse methods may not work, use square bracket syntax instead."
    warning(paste(strwrap(msg, indent = 0, exdent = 2), collapse = "\n"))
  }

  ## Done. Return result
  invisible(res)

}
