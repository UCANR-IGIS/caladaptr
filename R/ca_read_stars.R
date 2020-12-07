#' Read a stars object from disk
#'
#' @param x A single file name
#' @param read_sidecar Whether to read the sidecar file if it exists, logical
#'
#' @details
#' This function can be used to read a tif file downloaded by \code{\link{ca_getrst_stars}}. It
#' is a lightweight wrapper around stars::read_stars(), with the ability to create a sidecar
#' file (also created by \code{\link{ca_getrst_stars}}) which restores all of the attributes of
#' the array dimensions that are not preserved by the tif format.
#'
#' @seealso \code{\link{ca_getrst_stars}}
#'
#' @importFrom stars read_stars

#' @export

ca_read_stars <- function(x, read_sidecar = TRUE) {

  if (!is.character(x) || length(x) != 1) stop("x must be a character vector of length 1")

  if (!file.exists(x)) stop("file does not exist")

  res <- read_stars(x)
  if (read_sidecar) {
    sidecar_fn <- gsub(".tif$", ".attr.rds", x)
    if (file.exists(sidecar_fn)) {
      attr_lst <- readRDS(sidecar_fn)
      for (one_attr in names(attr_lst)) {
        attr(res, one_attr) <- attr_lst[[one_attr]]
      }
    }

  }

  invisible(res)

}
