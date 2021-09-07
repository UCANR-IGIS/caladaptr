big_geom <- function(type = 1) {

  library(sf)
  library(caladaptr)
  library(magrittr)

  epsg_utm11n_wgs84 <- 32611

  if (type == 1) {
    ## Get the top of the San Bernadino
    sb_bbox_top <- 35.80939

    socal_counties_idx <- ca_aoipreset_geom("counties") %>%
      st_centroid() %>%
      st_transform(4326) %>%
      st_coordinates() %>%
      subset(select=2) %>%
      is_less_than(sb_bbox_top) %>%
      which()

    ca_aoipreset_geom("counties") %>%
      slice(socal_counties_idx) %>%
      st_transform(epsg_utm11n_wgs84) %>%
      select(name, fips)

  } else if (type == 2) {
    ## 18k mi^2
    pt_ul <- c(-122.307696, 40.605256)
    pt_lr <- c(-119.810727, 38.583009)
    mat_x <- c(pt_ul[1], pt_lr[1], pt_lr[1], pt_ul[1], pt_ul[1])
    mat_y <- c(pt_ul[2], pt_ul[2], pt_lr[2], pt_lr[2], pt_ul[2] )
    rect_xy <- matrix(data=c(mat_x,mat_y), byrow = FALSE, ncol = 2)
    p <- st_polygon(x = list(rect_xy), dim = "XY")
    st_sf(id = 1, geom = st_sfc(list(p), crs = 4326))

  } else if (type == 3) {
    ## Area is 75k m^2
    pt_ul <- c(-124.614704, 42.069549)
    pt_lr <- c(-119.932053, 37.672699)
    mat_x <- c(pt_ul[1], pt_lr[1], pt_lr[1], pt_ul[1], pt_ul[1])
    mat_y <- c(pt_ul[2], pt_ul[2], pt_lr[2], pt_lr[2], pt_ul[2] )
    rect_xy <- matrix(data=c(mat_x,mat_y), byrow = FALSE, ncol = 2)
    p <- st_polygon(x = list(rect_xy), dim = "XY")
    st_sf(id = 1, geom = st_sfc(list(p), crs = 4326))

  } else if (type == 4) {
    ## Area in northern oR - outside Cal-Adapt area
    pt_ul <- c(-118.666353, 45.856491)
    pt_lr <- c(-117.184887, 44.908993)
    mat_x <- c(pt_ul[1], pt_lr[1], pt_lr[1], pt_ul[1], pt_ul[1])
    mat_y <- c(pt_ul[2], pt_ul[2], pt_lr[2], pt_lr[2], pt_ul[2] )
    rect_xy <- matrix(data=c(mat_x,mat_y), byrow = FALSE, ncol = 2)
    p <- st_polygon(x = list(rect_xy), dim = "XY")
    st_sf(id = 1, geom = st_sfc(list(p), crs = 4326))
  }


}



