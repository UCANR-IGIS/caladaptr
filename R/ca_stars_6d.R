#' Create a six-dimensional stars object for modeled climate data
#'
#' @param stars_lst A list of stars rasters
#' @param index_tbl A tibble of metadata for stars_lst
#'
#' @details stars_lst is a list of stars objects downloaded by \code{\link{ca_getrst_stars}} and turned into a list by
#' \code{\link{ca_stars_read}}. Note that both of these functions must use `sidecar = TRUE`.
#'
#' Creating a six-dimensional stars array of projected climate data may be useful for writing more compact expressions for
#' analysis. Six-dimensional arrays can only be constructed if the API request specifed the GCM, scenario, and climate variable.
#' Rasters retrieved using an API request that specified the dataset by the name of the slug can not be turned into a
#' 6D arrays. Another requirement is that all the rasters have the same location / extent.
#'
#' @return A six-dimensional stars object with dimensions x, y, scenario, gcm, date, and cvar
#'
#' @seealso \code{\link{ca_getrst_stars}}, \code{\link{ca_stars_read}}, \code{\link{ca_stars_index}}
#'
#' @importFrom dplyr select filter pull slice
#' @importFrom stars st_redimension st_set_dimensions
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#' }

## TODO: inherit the common ca_metadata attributes from the elements of stars_lst

ca_stars_6d <- function(stars_lst, index_tbl = NULL) {

  if (is.null(index_tbl)) index_tbl <- ca_stars_index(stars_lst)

  ## Check that the period is the same
  if (index_tbl %>% pull(period) %>% unique() %>% length() > 1) {
    stop("Can only create a 6D Cal-Adapt modelled data cube if all rasters have the same period")
  }

  ## Check that these are not LIVNEH
  if (TRUE %in% (index_tbl %>% pull(livneh))) {
    stop("Sorry, Livneh data can not be transformed into a 6D data cube")
  }

  ## Check that GCM and SCEN are available
  if (index_tbl %>% pull(gcm) %>% anyNA()) {
    stop("GCM not found (maybe the API request used a slug to specify the data?). These layers can not be transformed into a 6D data cube")
  }
  if (index_tbl %>% pull(scenario) %>% anyNA()) {
    stop("Scenario not found (maybe the API request used a slug to specify the data). These layers can not be transformed into a 6D data cube")
  }

  ## Create an empty list to hold the stars object we will eventually combine
  master_lst <- list()

  ## Loop thru scenarios
  for (scen_this in index_tbl %>% pull(scenario) %>% unique()) {

    # cat("Now working on scenario:", scen_this, "\n")

    ## Set up some objects to store products of the loop
    same_scen_lst <- list()

    ## Loop thru GCMs here
    for (gcm_this in index_tbl %>% filter(scenario == scen_this) %>%  pull(gcm) %>% unique()) {

      ## Get the list elements for this GCM and scenario
      same_scen_gcm_idx <- index_tbl %>%
        filter(gcm == gcm_this, scenario == scen_this) %>%
        pull(idx)

      ## Get the climate variable(s) for these list elements
      cvars_this <- index_tbl %>% slice(same_scen_gcm_idx) %>% pull(cvar)

      if (length(cvars_this) > 1) {
        ## Use c() to combine two+ stars objects, creating a new 'cvar' dimension to store the cvar name for each
        stars_same_scen_gcm <- do.call(c, c(stars_lst[same_scen_gcm_idx],
                                            list(along = list(cvar = cvars_this)))) %>%
          setNames("val")
      } else {
        ## There's only one stars object. We have to manually add the dimension and define its properties
        new_dims <- c(dim(stars_lst[[same_scen_gcm_idx]]),1)
        stars_same_scen_gcm <- stars_lst[[same_scen_gcm_idx]] %>%
          st_redimension(new_dims = new_dims) %>%
          st_set_dimensions(which = length(new_dims), values = cvars_this, names = "cvar") %>%
          st_set_dimensions(xy = c("x","y")) %>%
          setNames("val")
      }

      ## Store this stars object in the same_scen_lst list
      same_scen_lst[[gcm_this]] <- stars_same_scen_gcm

    }  ## done looping thru GCMs

    ## Combine all the GCMs stars with the GCM name(s) as a the values for a new 'gcm' dimension
    if (length(same_scen_lst) > 1) {
      ## Use c() to combine two+ stars objects, creating a new 'gcm' dimension to store the GCM for each
      master_lst[[scen_this]] <- do.call(c,
                                         c(same_scen_lst,
                                           list(along = list(gcm = names(same_scen_lst)))))
    } else {
      ## There's only one stars object. We have to manually add the dimension and define its properties
      new_dims <- c(dim(same_scen_lst[[1]]),1)
      master_lst[[scen_this]] <- same_scen_lst[[1]] %>%
        st_redimension(new_dims = new_dims) %>%
        st_set_dimensions(which = length(new_dims), values = names(same_scen_lst), names = "gcm") %>%
        st_set_dimensions(xy = c("x","y"))
    }

  } ## done looping thru scenarios

  ## Combine the one or two stars objects in master_lst, also change the order
  if (length(master_lst) > 1) {
    do.call(c,
            c(master_lst,
              list(along = list(scenario = names(master_lst))))) %>%
      aperm(c(1,2,6,5,3,4))

  } else {
    ## There's only one stars object. We have to manually add the dimension and define its properties
    new_dims <- c(dim(master_lst[[1]]),1)
    master_lst[[1]] %>%
      st_redimension(new_dims = new_dims) %>%
      st_set_dimensions(which = length(new_dims), values = names(master_lst), names = "scenario") %>%
      st_set_dimensions(xy = c("x","y")) %>%
      aperm(c(1,2,6,5,3,4))
  }

}
