#' Create an index for a list of stars rasters
#'
#' @param x A list of stars rasters
#'
#' @details
#' When you download rasters from Cal-Adapt using \code{\link{ca_getrst_stars}}
#' %>% \code{\link{ca_stars_read}}, you typically wind up with a list of stars rasters.
#' \code{ca_stars_index} generates an index of the properties of the elements of a list
#' of stars rasters to help identify which stars rasters contain which climate model data.
#'
#' @return A tibble with the properties of elements of \code{x}. There will be one row for
#' each element of x. Columns include cvar, scenario, gcm, period, slug, livneh, start, end,
#' rows, and cols.
#'
#' @seealso \code{\link{ca_getrst_stars}}, \code{\link{ca_stars_read}}, \code{\link{ca_stars_6d}}
#'
#' @importFrom purrr attr_getter pluck
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## Download 5 years of daily max and min temp for Merced County as rasters
#' mercd_cap <- ca_loc_aoipreset(type = "counties", idfld = "fips", idval = "06047") %>%
#'   ca_gcm(gcms[1:2]) %>%
#'   ca_period("day") %>%
#'   ca_cvar(c("tasmin", "tasmax")) %>%
#'   ca_scenario("rcp45") %>%
#'   ca_years(start = 2060, end = 2065)
#'
#' mercd_stars_lst <- mercd_cap %>%
#'   ca_getrst_stars(out_dir = ".") %>%
#'   ca_read_stars()
#'
#' ## Create an index tibble to see the climate model in each stars raster
#' mercd_stars_tbl <- mercd_stars_lst %>%
#'   ca_starslist_index()
#' }

ca_stars_index <- function(x) {

  msg_err <- "Expecting a list of stars rasters"

  if (!is.list(x)) stop(msg_err)
  if (!is(x[[1]], "stars")) stop(msg_err)

  if (!"ca_metadata" %in% names(attributes(x[[1]]))) stop("The ca_metadata attribute not found in these stars raster(s). Make sure sidecar_write=TRUE when you run ca_getrst_stars.")

  ## Create a function to extract a named attribute
  get_ca_metadata <- attr_getter("ca_metadata")

  ## Construct and return a tibble
  do.call(rbind, lapply(1:length(x),
                        function(i) pluck(x, i, get_ca_metadata) %>% as_tibble())) %>%
    mutate(idx = 1:length(x), .before = cvar)

}
