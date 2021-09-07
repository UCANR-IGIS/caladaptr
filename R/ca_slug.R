#' Adds slug(s) to a Cal-Adapt API call
#'
#' Specify the raster series slug a Cal-Adapt API call should retrieve
#'
#' @param x Cal-Adapt API request
#' @param slug Raster series slug(s)
#'
#' @details
#' To find valid slugs, see \code{\link{ca_catalog_rs}}.
#'
#' @seealso \code{\link{ca_catalog_rs}}
#'
#' @export

ca_slug <- function(x = ca_apireq(), slug) {

  if (!inherits(x, "ca_apireq")) stop("x should be an object of class ca_apireq")

  ## Error check
  if (!identical(slug, NA)) {
    all_slugs <- ca_catalog_rs(quiet = TRUE)$slug
    if (FALSE %in% (slug %in% all_slugs)) {
      stop(paste0("Unknown value(s) in slug: ",
                  paste0(slug[!slug %in% all_slugs], collapse = ", " ),
                  ". For valid values, run `View(ca_catalog_rs())`."))
    }
  }

  x$slug <- slug
  invisible(x)

}

