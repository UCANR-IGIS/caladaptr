#' Split a large geom into blocks small enough to ask the API for rasters
#'
#' @param x A big geom
#' @param block_area_mi2 Numeric area in square miles for each block.
#'   Smaller values => more sub-polygons. Default = 10,000.
#'
#' @details The Cal-Adapt API has a limit of around 20,000 mi^2 as the maximum area for which you can download a raster.
#' This function will take a sf data frame larger than this and return blocks that cover the same extent. Subsequently you can
#' download rasters for the individual blocks and mosaic them into the full area using ca_stars_mosaic.
#'
#' Note while this function can help you work around the maximum area you can download tifs via the API, it won't help you get
#' spatially aggregated values from a large area using the API. For that, you would need to a) use this function to download rasters,
#' b) mosaic them, and c) do a spatial aggregration on the large area-of-interest.
#'
#' If your study areas encompasses the entire Cal-Adapt coverage area, you'd be better off downloading the individual rasters
#' from the [Cal-Adapt Data Server](http://albers.cnr.berkeley.edu/data/).
#'
#' @return A polygon simple feature data frame covering the same extent as \code{x}
#'
#' @seealso \code{\link{ca_getrst_stars}}, \code{\link{ca_stars_read}}
#'
#' @importFrom units set_units
#' @importFrom sf st_geometry st_transform st_bbox st_polygon st_sfc st_sf st_intersects
#' @importFrom magrittr is_greater_than
#' @importFrom dplyr filter mutate
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#' }
ca_biggeom_blocks <- function(x, block_area_m2 = 10000) {

  ## Define the target area and width in m2
  target_area_m2 <- set_units(block_area_m2, "mi^2") %>% set_units("m^2") ##  51,800 km2
  target_width_m <- target_area_m2 %>% sqrt() %>% round()         ## about 228 km

  ## We'll use web mercator (units = m)
  epsg_webmerc <- 3857

  ## Get the bounding box of the input geom
  geom_bbox <- x %>%
    st_geometry() %>%
    st_transform(epsg_webmerc) %>%
    st_bbox()

  ## Compute the dimensions of the input geom bounding box
  geom_bbox_width_m <- as.numeric(geom_bbox[3] - geom_bbox[1])
  geom_bbox_height_m <- as.numeric(geom_bbox[4] - geom_bbox[2])

  ## Compute the number of columns we need based on the target width for a single Cal-Adapt API call
  num_cols <- ceiling(geom_bbox_width_m / as.numeric(target_width_m))

  ## Compute the x-points for the rectangle nodes for the block grid
  xs <- round(seq(from = geom_bbox[1], to = geom_bbox[3], length.out = num_cols))

  ## Compute the height of the blocks given the target area and the actual width
  block_target_height_m <- floor(as.numeric(target_area_m2) / (geom_bbox_width_m / num_cols))

  ## Compute the number of rows needed
  num_rows <- ceiling(geom_bbox_height_m / as.numeric(block_target_height_m))

  ## Compute the ys
  ys <- geom_bbox[2] + (0:(num_rows-1) * block_target_height_m)
  ys[length(ys)+1] <- geom_bbox[4]

  ## Construct a list of polygon objects
  p_lst <- list()
  for (j in (length(ys) - 1):1) {
    for (i in 1:(length(xs) - 1)) {
      mat_x <- xs[c(i,i+1,i+1,i,i)]
      mat_y <- ys[c(j+1,j+1,j,j,j+1)]
      p <- st_polygon(x = list(matrix(data=c(mat_x,mat_y), byrow = FALSE, ncol = 2)), dim = "XY")
      p_lst[[length(p_lst)+1]] <- p
    }
  }

  ## Create the sfc
  p_sfc <- st_sfc(p_lst, crs = epsg_webmerc)

  ## Create a preliminary sf object
  blocks_all_sf <- st_sf(id = 1:length(p_lst), geom = p_sfc) %>% st_transform(st_crs(x))

  ## Test which blocks intersect at least one feature of x
  blocks_keep_yn <- blocks_all_sf %>%
    st_intersects(x, sparse = FALSE) %>%
    apply(MARGIN = 1, sum) %>%
    is_greater_than(0)

  ## Return those blocks that intersect the input geometry (only)
  blocks_all_sf %>%
    filter(blocks_keep_yn) %>%
    mutate(id = paste0("b",  sprintf("%02d", 1:sum(blocks_keep_yn))))

}
