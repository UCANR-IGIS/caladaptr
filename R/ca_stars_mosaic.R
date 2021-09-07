#' Mosaic stars objects into a seamless array for large areas
#'
#' @param stars_lst A list of stars rasters
#' @param index_tbl A tibble of metadata for stars_lst
#' @param geom_mask A sf or sfc polygon object to crop the mosaiced raster
#' @param combine_6d Combine multiple 3D rasters into one 6D raster
#' @param quiet Suppress messages
#'
#' @details stars_lst is a list of stars objects downloaded by \code{\link{ca_getrst_stars}} and turned into a list by
#' \code{\link{ca_stars_read}}. Note that both of these functions must be run with `sidecar = TRUE` (the default).
#'
#' \code{combine_6d = TRUE} will return the mosaic as a six-dimensional raster. This further requires that the raster was
#' originally downloaded using an API request that specified the dataset by scenario, GCM, and climate variable (i.e., not
#' by a slug).
#'
#' @return If \code{combine_6d = FALSE}, a list of 3D rasters will be returned (x, y and date/year). If \code{combine_6d = TRUE}, a 6D stars
#' raster will be returned (x, y, date/year, scenario, GCM, climate variable).
#'
#' @seealso \code{\link{ca_getrst_stars}}, \code{\link{ca_stars_read}}, \code{\link{ca_stars_index}}
#'
#' @importFrom dplyr select filter pull
#' @importFrom stars st_mosaic st_dimensions st_set_dimensions st_dimensions<-
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }

ca_stars_mosaic <- function(stars_lst, index_tbl = NULL, geom_mask = NULL, combine_6d = FALSE, quiet = FALSE) {

  if (is.null(index_tbl)) index_tbl <- ca_stars_index(stars_lst)

  ## Check that the period is the same
  if (index_tbl %>% pull(period) %>% unique() %>% length() > 1) {
    stop("Can only create a 6D Cal-Adapt modelled data cube if all rasters have the same period")
  }

  if (combine_6d) {
    ## Make sure the units are the same
    ## if () stop("Units have to be the same...")

    ## Initialize an index table that we will build up with rbind() within the loop
    mosaic_index_tbl <- NULL
  }

  ## Define the formatting function for messages
  accent2 <- getOption("ca_accent2", I)

  ## Create an empty list to hold the individual stars mosaics
  master_lst <- list()

  ## Get a vector of unique slugs for looping
  slugs_unique <- index_tbl$slug %>% unique()

  ## (slug_this <- slugs_unique[1])
  for (slug_this in slugs_unique) {

    ## Get the list elements for this slug
    stars_this_mosaic_idx <- index_tbl %>%
      filter(slug == slug_this) %>%
      pull(idx)

    if (!quiet) {
      message(accent2(paste0(" - mosaicing ", length(stars_this_mosaic_idx), " stars rasters for ", slug_this)))
    }

    ## Save a row of metadata
    if (combine_6d) {
      mosaic_index_tbl <- mosaic_index_tbl %>%
        rbind(index_tbl %>%
                filter(slug == slug_this) %>%
                slice(1) %>%
                select(cvar, scenario, gcm, period, slug, livneh, start, end))
    }

    ## Make a copy of the third dimension of the first stars raster, we'll clone this later
    first_star_third_dimension <- st_dimensions(stars_lst[[stars_this_mosaic_idx[1]]])[3]

    ## Mosaic all the stars rasters for this layer, and manually change the name of the 3rd
    ## dimension to match the original ('year' or 'date')
    master_lst[[slug_this]] <- do.call(st_mosaic, stars_lst[stars_this_mosaic_idx]) %>%
      st_set_dimensions(which = 3, names = names(first_star_third_dimension)) %>%
      setNames(slug_this)

    ## Copy the properties of the 3rd dimension to the mosaic
    st_dimensions(master_lst[[slug_this]])[3] <- first_star_third_dimension

    if (!is.null(geom_mask)) {
      master_lst[[slug_this]] <- master_lst[[slug_this]] %>% st_crop(geom_mask)
    }

  }  ## for slug_this in slugs_unique

  if (combine_6d) {

    ## Add idx to mosaic_index_complete_tbl
    mosaic_index_complete_tbl <- mosaic_index_tbl %>%
      mutate(idx = 1:nrow(mosaic_index_tbl), .before = "cvar") %>%
      mutate(idfld = NA, idval = NA)

    ## Return a 6D stars object
    ca_stars_6d(master_lst, mosaic_index_complete_tbl)

  } else {
    ## Return the list of 3D mosaics
    master_lst
  }

}
