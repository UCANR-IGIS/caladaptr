#' Example API requests
#'
#' @param n Which sample API request to return (1..5)
#'
#' @details
#'
#' These sample API requests can be used in demos, documentation, and tests.
#'
#' n = 1: Basic API request - one point, two CGMs, 20 years of annual data.
#' n = 2: Three Congressional districts, monthly data, 4 years
#' n = 3: sf data frame with one feature, 1 GCM, 1 scenario, 2 years of daily data
#' n = 4: Simple feature data frame with two multipolygons, 1 GCM, 1 scenario, 20 years of annual data
#'
#' @importFrom sf st_read st_sf
#' @export


ca_example_apireq <- function(n) {

  if (!is.numeric(n) || length(n) != 1) stop("n must be a number between 1 and 5")

  if (n == 1) {
    ## Basic API request - one point, two CGMs, 20 years of annual data
    ca_loc_pt(coords = c(-121.4687, 38.5938)) %>%
      ca_gcm(gcms[1:2]) %>%
      ca_scenario(scenarios[1]) %>%
      ca_period("year") %>%
      ca_years(start = 2040, end = 2060) %>%
      ca_cvar("tasmax")


  } else if (n == 2) {
    ## Congressional districts with monthly date from a slug for 4 years
    ca_loc_aoipreset(type = "cdistricts",
                     idfld = "geoid",
                     idval = c("0620", "0624", "0626")) %>%
      ca_slug("baseflow_month_CMCC-CMS_rcp85") %>%
      ca_dates(start = "2080-01-01", end = "2083-12-31") %>%
      ca_options(spatial_ag = "mean")

  } else if (n == 3) {
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

  } else if (n == 4) {

    ## Simple feature data frame with two multipolygons, 1 GCM, 1 scenario, 20 years of annual data
    cnty_sf <- st_read(system.file("extdata", "county_bnd.geojson", package = "caladaptr"),
                       quiet = TRUE)

    ca_loc_sf(loc = cnty_sf, idfld = "name") %>%
      ca_gcm(gcms[9]) %>%
      ca_scenario(scenarios[1]) %>%
      ca_period("year") %>%
      ca_years(start = 2040, end = 2060) %>%
      ca_cvar("tasmin") %>%
      ca_options(spatial_ag = "mean")

  } else if (n == 5) {


  } else {
    stop("Unknown value of n")
  }

}


