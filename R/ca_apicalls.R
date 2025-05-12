#' Creates a tibble of individual API calls
#'
#' Constructs a tibble of individual API calls for an API request
#'
#' @param x A Cal-Adapt API request
#' @param slug_check Cross check the slug against the raster series catalog
#' @param date_check Cross check the start and end date against the raster series catalog
#' @param loc_check Check locations for duplicate values and the Cal-Adapt coverage area
#' @param units_check Check for consistent units
#' @param spag_check Check spatial aggregation option
#' @param check_for Which operations or context to use
#' @param pf Run as a preflight check only, logical
#' @param ignore_spag Deprecated
#' @param preflight Deprecated
#'
#' @details This is an internal function which a) evaluates an API request and generates a tibble
#' of individual API calls, and b) checks for errors. It is exported as it may have some use for
#' trouble-shooting and developers, but is not something most users will need.
#'
#' This function is called in two contexts:
#'
#' a) by ca_preflight() which will report the errors to the user
#'
#' b) by data fetching functions (ca_getvals_tbl, ca_getvals_db, and ca_getrst_stars), which loop through the
#' tibble returned to fetch the data
#'
#' @returns
#' If \code{pf = TRUE}, a list of three types of errors / issues discovered: i) general errors, ii) problems
#' specific to querying values, and iii) problems specific to downloading rasters.
#'
#' @importFrom dplyr select mutate left_join right_join pull
#' @importFrom tibble tibble
#' @importFrom sf st_as_text st_point st_transform st_simplify st_read st_intersects st_as_sf st_disjoint st_within st_area st_geometry_type
#' @importFrom httr modify_url
#' @importFrom crayon green yellow
#' @importFrom utils URLencode
#' @importFrom digest digest2int digest
#' @importFrom lifecycle is_present deprecate_warn deprecated
#'
#' @keywords internal
#' @export

ca_apicalls <- function(x, slug_check = TRUE, date_check = TRUE, loc_check = TRUE,
                        units_check = TRUE, spag_check = FALSE, pf = FALSE,
                        check_for = c("getvals", "getrst"), ignore_spag = deprecated(), preflight = deprecated()) {

  if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq", call. = FALSE)

  if (FALSE %in% (check_for %in% c("getvals", "getrst"))) stop("unknown value(s) for `check_for`")

  if (is_present(ignore_spag)) {
    deprecate_warn("0.6.0", "ca_apicalls(ignore_spag)", "ca_apicalls(spag_check)")
    spag_check <- !ignore_spag
  }

  if (is_present(preflight)) {
    deprecate_warn("0.6.0", "ca_apicalls(preflight)", "ca_apicalls(pf)")
    pf <- preflight
  }

  ## Create variables to store messages about errors and issues discovered
  ## These will be populated by calls to stopwarn()
  genl_msgs <- character(0)  ## general messages
  gval_msgs <- character(0)  ## messages specific to using the API request to get values
  grst_msgs <- character(0)  ## messages specific to using the API request to get rasters

  check_duplicate_hash_int <- TRUE

  ## Error checks on the API request (to be moved to its own function)
  if (identical(x$loc, NA)) stopwarn(pf, check_for, stop_msg="A location must be specified")

  ## Verify that either slug *or* gcm+per+scenario+cvar were passed *or* livneh+per+cvar
  if (identical(x$slug, NA)) {

    if (identical(x$per, NA)) stopwarn(pf, check_for, stop_msg="A period must be specified")
    if (identical(x$cvar, NA)) stopwarn(pf, check_for, stop_msg="Climate variable must be provided")

    if (identical(x$livneh, TRUE)) {
      if (!identical(x$gcm, NA)) stopwarn(pf, check_for, stop_msg="An API request can't specify both Livneh and a GCM.")
      if (!identical(x$scenario, NA)) stopwarn(pf, check_for, stop_msg="An API request can't specify both Livneh and an emissions scenario.")
      check_duplicate_hash_int <- FALSE
    } else {
      if (identical(x$gcm, NA)) stopwarn(pf, check_for, stop_msg="A GCM must be specified")
      if (identical(x$scenario, NA)) stopwarn(pf, check_for, stop_msg="An emissions scenario must be specified")
    }

  } else {

    if (!identical(x$gcm, NA) || !identical(x$scenario, NA) || !identical(x$per, NA) || !identical(x$cvar, NA) ||
        ifelse(is.null(x$livneh), FALSE, !identical(x$livneh, NA))) {
      stopwarn(pf, check_for, stop_msg="Slug can not be combined with gcm, period, scenario, cvar, or livneh. Specify the dataset with one or the other.")
    }

  }

  ## Load the loca grid polygon
  if (loc_check) {
    loca_area_sf <- st_read(system.file("extdata", "loca_area.geojson", package = "caladaptr"), quiet = TRUE)
  }

  ## THE FIRST THING WE DO IS CREATE A TIBBLE FOR ALL THE LOCATIONS, WITH COLUMNS THAT WILL BE NEEDED
  ## TO BUILD THE QUERY PARAMETERS.
  ## Columns: loc_type, loc_fld, feat_id, loc_qry (either g= or ref= or the row number of a sf data frame)
  ## (the order of these columns matters a bit - feat_id should be first because it will be renamed down the road)

  if (x$loc$type == "pt") {

    loc_tbl <- tibble(feat_id = x$loc$val[, 1],
                      loc_type = factor("pt"),
                      loc_preset = factor(NA),
                      loc_fld = factor(NA),
                      loc_qry = paste0("g=",
                                       sapply(1:nrow(x$loc$val),
                                              function(i) st_as_text(st_point(as.numeric(x$loc$val[i, c(2,3)]))))))

    ## Check for duplicate coordinates
    dup_loc <- anyDuplicated(x$loc$val[ ,2:3]) !=0
    if (dup_loc) check_duplicate_hash_int <- FALSE

    if (loc_check) {
      ## Verify the point(s) are in the Cal-Adapt area
      qry_pts_sf <- st_as_sf(x$loc$val[ ,2:3], coords = c("x","y"), crs = 4326)

      suppressMessages({
        pts_in_loca_area_mat <- qry_pts_sf %>% st_intersects(loca_area_sf, sparse = FALSE)
      })
      if (FALSE %in% pts_in_loca_area_mat) {
        stopwarn(pf, check_for, stop_msg="One or more points fall outside the area covered by Cal-Adapt")
      }

      if (dup_loc) stopwarn(pf, check_for, stop_msg="Duplicate locations found.")

    }

    idfld_name <- names(x$loc$val)[1]
    loc_sf <- NA
    sf_hash <- ""

  } else if (x$loc$type == "aoipreset") {

    ## If idval = NULL, fill idval with all valid values
    if (is.null(x$loc$val$idval)) {
      x$loc$val$idval <- aoipreset_idval[[x$loc$val$type]][[x$loc$val$idfld]]
    }

    preset <- as.character(x$loc$val$type)
    idfld <- as.character(x$loc$val$idfld)
    idvals <- x$loc$val$idval
    idfld_name <- idfld

    ## Get the matching values of the 'id' column (which is what we have to put in the URL)
    api_ids <- tibble(!!idfld := idvals) %>%
      left_join(aoipreset_idval[[preset]], by = idfld) %>%
      pull(id)

    ## They *should* all have a match, but check just in case
    if (anyNA(api_ids)) stopwarn(pf, check_for, stop_msg=paste0("Unknown value(s) found in ", idfld))

    ## Construct a tibble for the location columns (order matters - feat_id should be first)
    loc_tbl <- tibble(feat_id = idvals,
                      loc_type = factor("aoipreset"),
                      loc_preset = factor(preset),
                      loc_fld = factor(idfld),
                      loc_qry = paste0("ref=/api/", preset, "/", api_ids, "/"))

    loc_sf <- NA
    sf_hash <- ""



  } else if (x$loc$type == "sf") {

    idfld_name <- as.character(x$loc$val$idfld)

    loc_sf <- x$loc$val$loc %>% st_transform(4326)

    if (x$loc$val$dTolerance > 0) {
      #size_before <- object.size(loc_sf)
      loc_sf <- loc_sf %>% st_simplify(dTolerance = x$loc$val$dTolerance)
      ## Savings: (size_before - object.size(loc_sf))
    }

    ## Generate loc_tbl. loc_qry stores the row number of each feature and will be used
    ## when fetching data.

    loc_tbl <- tibble(feat_id = x$loc$val$idval,
                      loc_type = factor("sf"),
                      loc_preset = factor(NA),
                      loc_fld = factor(idfld_name),
                      loc_qry = 1:nrow(x$loc$val$loc))

    ## Verify the features intersect in the Cal-Adapt area
    if (loc_check) {

      if ("getvals" %in% check_for) {
        ## First check whether the features are within the loca coverage area
        suppressMessages({
          feats_within_loca_area_mat <- x$loc$val$loc %>% st_transform(4326) %>% st_within(loca_area_sf, sparse = FALSE)
        })
        if (FALSE %in% feats_within_loca_area_mat) {
          stopwarn(pf, check_for, stop_msg="One or more features lies outside Cal-Adapt's coverage area",
                   gval_msg="One or more features may fall outside Cal-Adapt's coverage area")
        }

      }  ## if getvals %in% check_for

      if ("getrst" %in% check_for) {
        ## When retrieving rasters, features that extend beyond the Cal-Adadpt loca grid are not a deal breaker, the API
        ## will simply mask values that have no values. However features that are completely outside the grid will generate an error and are trapped
        suppressMessages({
          feats_disjoint_loca_area_mat <- x$loc$val$loc %>% st_transform(4326) %>% st_disjoint(loca_area_sf, sparse = FALSE)
        })
        if (TRUE %in% feats_disjoint_loca_area_mat) {
          stopwarn(pf, check_for, stop_msg="One or more features falls completely outside Cal-Adapt's coverage area",
                   grst_msg="One or more features may fall completely outside Cal-Adapt's coverage area")
        }

      }  ## if getrst %in% check_for


      ## Next check the toal size of the area of interest
      epsg_caalbers <- 3310
      max_area_m2 <- 52079387300 ## Size of San Bernadino Cty is known to work. Exactly 20k mi^2 = 51799762207
      if (TRUE %in% (x$loc$val$loc %>% st_transform(epsg_caalbers) %>% st_area() %>% as.numeric() > max_area_m2)) {
        stopwarn(pf, check_for, stop_msg="One or more features might be too big for the Cal-Adapt API. Consider `ca_biggeom_split()`")
      }
    }

    ## We create a hash string of the sf object which will be incorporated when computing the hash for each API call.
    ## This is to differentiate API calls for two different SF objects that have identical id field names and values
    sf_hash <- digest(loc_sf, serialize = TRUE)


  } else if (x$loc$type == "zip") {
    stopwarn(pf, check_for, stop_msg="Sorry, querying by zip code is not yet supported")
    loc_sf <- NA
    sf_hash <- ""


  } else {
    stopwarn(pf, check_for, stop_msg="Unknown value for location type!!")
    loc_sf <- NA
    sf_hash <- ""

  }

  ## Add a column for the row number, which we'll need later to join
  loc_tbl <- loc_tbl %>%  mutate(loc_idx = 1:nrow(loc_tbl))

  ## Next, create a tibble for all permutations of elements that specify the raster series:
  ## cvar, period, gcm, scenario, slug, rs_name, and rs_units
  rs_tbl <- tibble(expand.grid(cvar = factor(x$cvar),
                               period = factor(x$period),
                               gcm = factor(x$gcm),
                               scenario = factor(x$scenario),
                               slug = as.character(x$slug),
                               livneh = identical(x$livneh, TRUE),
                               stringsAsFactors = FALSE))

  ## If the API request does not specify slug(s), construct those now
  if (identical(x$slug, NA)) {

    ## Construct a list of the slug 'suffix' for Livneh layers
    ## This is to mimic how these slugs were constructed on Cal-Adapt
    slug_suffix_lst <- setNames(as.list(rep("", length(cvars))), cvars)
    slug_suffix_lst[!names(slug_suffix_lst) %in% c("pr", "tasmax", "tasmin")] <- "_vic"

    rs_tbl <- rs_tbl %>%
      mutate(slug = if_else(livneh,
                            paste0(paste(cvar, period, "livneh", sep="_"), slug_suffix_lst[as.character(cvar)]),
                            paste(cvar, period, gcm, scenario, sep="_")))

  }

  ## Add a lower case version of the slug for joining to the raster series data catalog
  rs_tbl <- rs_tbl %>% mutate(slug_lower = tolower(slug))

  ## Grab a few additional columns from the raster series data catalog for the slug check and units assignment
  rs_catinfo_df <- ca_catalog_rs(quiet = TRUE) %>%
    mutate(slug_lower = tolower(slug)) %>%
    select(slug_lower, rs_name = name, rs_units = units, rs_begin = begin, rs_end = end, tres = tres)

  ## Do the join
  rs_tbl_matches <- rs_tbl %>% left_join(rs_catinfo_df, by = "slug_lower")

  if (slug_check) {
    if (anyNA(rs_tbl_matches$rs_name)) {
      stopwarn(pf, check_for, stop_msg=paste0("Unknown slug(s): ",
                  paste(rs_tbl_matches %>% filter(is.na(rs_name)) %>% pull(slug),
                        collapse = ", ")))
    }
  }

  ## Add a column with the row numbers that we'll use later for a join
  rs_tbl <- rs_tbl %>%
    mutate(rs_idx = 1:nrow(rs_tbl)) %>%
    left_join(rs_catinfo_df, by = "slug_lower")

  ## Check that all API calls return the same units
  if (units_check) {
    ## Different units is an issue for getting values (because they all go into the same tibble, but not rasters)
    if ("getvals" %in% check_for) {
      if (length(unique(rs_tbl$rs_units)) > 1) stopwarn(pf,
                                                        check_for,
                                                        stop_msg=paste0("The variables in this API request return different units (",
                                                                        paste(unique(rs_tbl$rs_units), collapse = ", "), "). The variables must all use the same units."),
                                                        gval_msg = "The climate variables use different units and can not be saved in the same table.")
    }
  }

  ## Prep the dates for the API request. These will be the same for all API calls.
  if (identical(x$dates, NA)) {
    start_dt <- NA
    end_dt <- NA
    dt_qry <- ""

  } else {
    start_dt <- x$dates$start
    end_dt <- x$dates$end
    dt_qry <- paste0("&start=", format(start_dt, format = "%Y-%m-%d"),
                     "&end=", format(end_dt, format = "%Y-%m-%d"))
  }

  ## Check the query dates against the raster series dates
  if (date_check && !identical(x$dates, NA)) {
    rs_latest_begin_dt <- max(as.Date(substr(rs_tbl_matches$rs_begin, 1, 10), format = "%Y-%m-%d"))
    rs_earliest_end_dt <- min(as.Date(substr(rs_tbl_matches$rs_end, 1, 10), format = "%Y-%m-%d"))

    ## Only proceed with the date check if there are values for rs_begin and rs_end (if not,
    ## the dataset probably doesn't exist in which case it'll be flagged in the slug check)
    if (!is.na(rs_latest_begin_dt) && !is.na(rs_earliest_end_dt)) {
      qry_begin_dt <- as.Date(format(start_dt, format = "%Y-%m-%d"))
      if (qry_begin_dt < rs_latest_begin_dt || qry_begin_dt > rs_earliest_end_dt) {
        stopwarn(pf, check_for, stop_msg="Start date beyond the range of at least one raster series")
      }

      qry_end_dt <- as.Date(format(end_dt, format = "%Y-%m-%d"))
      if (qry_end_dt < rs_latest_begin_dt || qry_end_dt > rs_earliest_end_dt) {
        stopwarn(pf, check_for, stop_msg="End date beyond the range of at least one raster series")
      }

    }


  }

  ## SPAG options - generate stat_tbl

  ## First determine if the area(s) of interest are polygons (for error trapping below)
  aoi_is_poly <- FALSE
  if (x$loc$type == "aoipreset") {
    aoi_is_poly <- TRUE
  } else if (x$loc$type == "sf") {
    if (FALSE %in% (unique(as.character(st_geometry_type(x$loc$val$loc))) %in% c("POINT", "MULTIPOINT"))) {
      aoi_is_poly <- TRUE
    }
  }

  ## Create a generic error message
  err_msg_spag <- "A spatial aggregation function is required to query values from polygon areas. See `ca_options`."

  ## If aoi_is_poly = TRUE, we need to catch if x$options is NA (i.e., ca_options was never called) or x$options$spatial_ag == 'none'
  ## These are only for getvals
  ## Warning will not be reported if check_spag is FALSE

  if (identical(x$options, NA)) {

    if (aoi_is_poly && ("getvals" %in% check_for) && spag_check) {
      ## AOI is a polygon and ca_options() was never called: Stop the show or flag a warning message
      stopwarn(pf, check_for, stop_msg=err_msg_spag, gval_msg = err_msg_spag)
    }

    ## Create stat_tbl with NULL values
    stat_tbl <- tibble(stat_idx = 1, spag = factor("none"), stat_qry = "")

  } else {

    ## x$options$spatial_ag exists (is not NA)

    ## We don't need to check if x$options$spatial_ag is NULL - that could only happen if
    ## someone manually edited the ca_apireq object

    spag <- x$options$spatial_ag

    ## Verify we have valid spatial aggregation function if aoi_is_poly = TRUE
    if (identical(spag, "none") && aoi_is_poly && ("getvals" %in% check_for) && spag_check) {
      stopwarn(pf, check_for, stop_msg=err_msg_spag, gval_msg = err_msg_spag)
    }

    stat_tbl <- tibble(stat_idx = 1:length(spag),
                       spag = factor(spag),
                       stat_qry = paste0("&stat=", spag))
  }

  ## Construct the "frame" for all permutations of location + slug + dates + spag
  apitbl_frame <- tibble(expand.grid(loc_idx = 1:nrow(loc_tbl),
                                     rs_idx = 1:nrow(rs_tbl),
                                     stat_idx = 1:nrow(stat_tbl),
                                     start = start_dt,
                                     end = end_dt,
                                     dt_qry = dt_qry))

  ## Next, join columns from the location tibble, raster series tibble, and stat_tbl.
  ## While we're at it, we'll compute a hash integer representing the key search parameters
  ## (to be used in ca_getvals_db() to record API calls already made)

  api_tbl <- loc_tbl %>%
    right_join(apitbl_frame, by = "loc_idx") %>%
    left_join(rs_tbl, by = "rs_idx") %>%
    left_join(stat_tbl, by = "stat_idx") %>%
    select(-rs_idx, -loc_idx, -stat_idx, -slug_lower) %>%
    mutate(hash_int = digest2int(paste(sf_hash, slug, loc_type, loc_preset, loc_fld, loc_qry, start, end, spag, sep =".")))

  ## Check for duplicate values in hash_int. This would mean the paste() equation above for uniquely identifying API calls is deficient.
  if (check_duplicate_hash_int) {
    if (anyDuplicated(api_tbl$hash_int) != 0) {
      stopwarn(pf, check_for, stop_msg="Duplicate values of `hash_int` found. This should not happen - please contact the package author.")
    }
  }

  ## Generate the full URL and an integer hash of all the search components
  if (x$loc$type == "sf") {
    ## If querying by sf object, all the parameters will go in the body, so we just need the URL
    api_tbl <- api_tbl %>%
      mutate(api_url = sapply(paste0(ca_baseurl, "series/", slug, "/events/"), URLencode))

  } else {

    api_tbl <- api_tbl %>%
      mutate(api_url = sapply(paste0(ca_baseurl, "series/", slug, "/events/?",
                                     loc_qry, dt_qry, stat_qry), URLencode))

  }

  # message(silver(" - TODO: cross-check raster series period vs period"))


  if (pf) {
    ## Return errors found
    invisible(list(genl_msgs = genl_msgs,
                   gval_msgs = gval_msgs,
                   grst_msgs = grst_msgs))

  } else {
    ## Prepare a list to return containing the api table and the sf object
    invisible(list(api_tbl = api_tbl,
                loc_sf = loc_sf,
                idfld = idfld_name))

  }

}


#' Internal function used by ca_apicalls() to handle issues discovered.
#'
#' This is mostly to keep the code clean.
#' If pf=TRUE, they'll just be logged in genl_msgs, gval_msgs, grst_msgs.
#' stop_msg is the message to show if the problem is a deal-breaker
#' gval_msgs, grst_msgs are the messages to show if the context is getvals or getrst (these are sometimes different)
#' If neither gval_msgs or grst_msgs are passed, the error will be logged (pf=TRUE) or code stopped (pf=FALSE)
#' If pf = FALSE and stop_msg has value, the stop() will be called
#'
#'
#' @keywords internal
stopwarn <- function(pf, check_for, stop_msg = NULL, gval_msg = NULL, grst_msg = NULL) {

  if (pf) {
    e0 <- parent.frame()
    ## Record the error message as a general or global error
    if (!is.null(stop_msg)  && is.null(gval_msg) && is.null(grst_msg)) {
      assign("genl_msgs", c(get("genl_msgs", envir = e0), stop_msg), envir = e0)
    }
    ## Record the error message as a get values problem
    if (!is.null(gval_msg) && ("getvals" %in% check_for)) {
      assign("gval_msgs", c(get("gval_msgs", envir = e0), gval_msg), envir = e0)
    }
    ## Record the error message as a get raster problem
    if (!is.null(grst_msg) && ("getrst" %in% check_for)) {
      assign("grst_msgs", c(get("grst_msgs", envir = e0), grst_msg), envir = e0)
    }

  } else {
    ## If a stop message is passed and this is not a preflight call, stop the presses
    if (!is.null(stop_msg)) stop(stop_msg, call. = FALSE)
  }

}




