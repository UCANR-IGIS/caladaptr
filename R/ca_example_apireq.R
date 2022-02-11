#' Sample API requests
#'
#' Sample API requests
#'
#' @param x The number of a sample API request to return
#'
#' @details
#'
#' These sample API requests can be used in demos, documentation, and tests. \code{x} should be an
#' integer:
#'
#' \code{x = 1}: Basic API request for Scripps data -- one point, 4 CGMs, 20 years of annual data.
#'
#' \code{x = 2}: Three Congressional districts, monthly data, 4 years
#'
#' \code{x = 3}: sf data frame with one feature, 1 GCM, 1 scenario, 2 years of daily data
#'
#' \code{x = 4}: sf data frame with two multipolygons, 1 GCM, 1 scenario, 20 years of annual data
#'
#' \code{x = 5}: Livheh data, ten census tracts, 20 years of daily temp data,
#' spatial aggregation mean
#'
#' \code{x = 6}: Livheh data, five census tracts (including one from #5), 5 years of daily temp
#' data, spatial aggregation = mean
#'
#' \code{x = 7}: Basic API request for Scripps data -- one point, 4 CGMs, 70 years of annual data.
#'
#' @importFrom sf st_read st_sf
#'
#' @export

ca_example_apireq <- function(x) {

  if (!is.numeric(x) || length(x) != 1) stop("x must be a number between 1 and 5")

  if (x == 1) {
    ## Basic API request - one point, two CGMs, 20 years of annual data
    ca_loc_pt(coords = c(-121.4687, 38.5938)) %>%
      ca_gcm(gcms[1:4]) %>%
      ca_scenario(scenarios[1]) %>%
      ca_period("year") %>%
      ca_years(start = 2040, end = 2060) %>%
      ca_cvar("tasmax")

  } else if (x == 2) {
    ## Congressional districts with monthly date from a slug for 4 years
    ca_loc_aoipreset(type = "cdistricts",
                     idfld = "geoid",
                     idval = c("0620", "0624", "0626")) %>%
      ca_slug("baseflow_month_CMCC-CMS_rcp85") %>%
      ca_dates(start = "2080-01-01", end = "2083-12-31") %>%
      ca_options(spatial_ag = "mean")

  } else if (x == 3) {
    ## Simple rectangular polygon, created from scratch
    pts_closed_mat <- matrix(data = c(-120.7635, -120.4285, -120.3873, -120.7377, -120.7635,
                                      38.90920, 38.91321, 38.66819, 38.66015, 38.90920),
                             ncol = 2)
    simp_poly_sf <- st_polygon(x = list(pts_closed_mat), dim = "XY") %>%
      st_sfc(crs = 4326) %>%
      data.frame(id = 1, geom = .) %>%
      st_sf()

    ca_loc_sf(loc = simp_poly_sf, idfld = "id") %>%
      ca_gcm(gcms[6]) %>%
      ca_scenario(scenarios[2]) %>%
      ca_period("day") %>%
      ca_years(start = 2010, end = 2012) %>%
      ca_cvar("pr") %>%
      ca_options(spatial_ag = "mean")

  } else if (x == 4) {

    ## Simple feature data frame with two multipolygons, 1 GCM, 1 scenario, 20 years of annual data
    cnty_sf <- st_read(system.file("extdata", "county_bnd.geojson", package = "caladaptr"), quiet = TRUE)

    ca_loc_sf(loc = cnty_sf, idfld = "name") %>%
      ca_gcm(gcms[9]) %>%
      ca_scenario(scenarios[1]) %>%
      ca_period("year") %>%
      ca_years(start = 2040, end = 2060) %>%
      ca_cvar("tasmin") %>%
      ca_options(spatial_ag = "mean")

  } else if (x == 5) {
    ## API request for Livheh data - ten census tracts, 20 years of daily temp data, spatial aggregation mean
    socal_tracts_int <- c(6025010101, 6025010102, 6025010200, 6025010300, 6025010400,
                          6025010500, 6025010600, 6025010700, 6025010800, 6025010900)
    sctracts_tas_liv_cap <- ca_loc_aoipreset(type="censustracts", idfld = "tract", idval = socal_tracts_int) %>%
      ca_livneh() %>%
      ca_years(start = 1990, end = 2010) %>%
      ca_cvar(c("tasmin", "tasmax")) %>%
      ca_period("day") %>%
      ca_options(spatial_ag = "mean")

  } else if (x == 6) {
    ## API request for Livheh data - 5 census tracts (first one overlaps with #5), 5 years of daily temp data.
    socal_tracts_int <- c(6025010900, 6025011100, 6025011201, 6025011202, 6025011300)
    ca_loc_aoipreset(type="censustracts", idfld = "tract", idval = socal_tracts_int) %>%
      ca_livneh() %>%
      ca_years(start = 1995, end = 2000) %>%
      ca_cvar(c("tasmin", "tasmax")) %>%
      ca_period("day") %>%
      ca_options(spatial_ag = "mean")


  } else if (x == 7) {
      ## Basic API request - one point, 4 CGMs, 70 years of annual data
      ca_loc_pt(coords = c(-119.0, 35.4)) %>%
        ca_gcm(gcms[1:4]) %>%
        ca_scenario("rcp85") %>%
        ca_period("year") %>%
        ca_years(start = 2030, end = 2099) %>%
        ca_cvar("tasmax")

  } else {
    stop("Unknown value of x")

  }

}


