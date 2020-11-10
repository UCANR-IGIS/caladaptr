#' Creates a tibble of individual API calls
#'
#' Constructs a tibble of individual API calls for an API request
#'
#' @param x A Cal-Adapt API request
#' @param slug_check Cross check the slug against the raster series catalog
#' @param date_check Cross check the start and end date against the raster series catalog
#'
#' @importFrom dplyr select mutate left_join right_join pull
#' @importFrom tibble tibble
#' @importFrom sf st_as_text st_point st_transform st_simplify
#' @importFrom httr modify_url
#' @importFrom crayon green yellow
#' @importFrom utils URLencode
#' @importFrom digest digest2int digest
#' @export

ca_apicalls <- function(x, slug_check = TRUE, date_check = TRUE) {

  if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq")

  ## Error checks on the API request (to be moved to its own function)
  if (identical(x$loc, NA)) stop("A location must be specified")

  ## Verify that either slug *or* gcm+per+scenario+cvar were passed
  if (identical(x$slug, NA)) {
    if (identical(x$gcm, NA)) stop("A gcm must be specified")
    if (identical(x$scenario, NA)) stop("An emissions scenario must be specified")
    if (identical(x$per, NA)) stop("A period must be specified")
    if (identical(x$cvar, NA)) stop("Climate variable must be provided")

  } else {

    if (!identical(x$gcm, NA) || !identical(x$scenario, NA) || !identical(x$per, NA) || !identical(x$cvar, NA)) {
      stop("In an API request, slug can not be combined with gcm, period, scenario, or cvar. Specify the raster series with one approach or the other.")
    }

  }

  ## THE FIRST THING WE DO IS CREATE A TIBBLE FOR ALL THE LOCATIONS, WITH COLUMNS THAT WILL BE NEEDED
  ## TO BUILD THE QUERY PARAMETERS.
  ## Columns: loc_type, loc_fld, feat_id, loc_qry (either g= or ref= or a geojson string)
  ## (the order of these columns matters - feat_id should be first because it will be renamed down the road)

  if (x$loc$type == "pt") {

    loc_tbl <- tibble(feat_id = x$loc$val[, 1],
                      loc_type = factor("pt"),
                      loc_preset = factor(NA),
                      loc_fld = factor(NA),
                      loc_qry = paste0("g=",
                                       sapply(1:nrow(x$loc$val),
                                       function(i) st_as_text(st_point(as.numeric(x$loc$val[i, c(2,3)]))))))

    idfld_name <- names(x$loc$val)[1]
    loc_sf <- NA
    gson_fn_base <- NA
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

    # idfld_lst <- list(name = idfld, class = class(idvals))

    ## Get the matching values of the 'id' column (which is what we have to put in the URL)
    api_ids <- tibble(!!idfld := idvals) %>%
      left_join(aoipreset_idval[[preset]], by = idfld) %>%
      pull(id)

    ## They *should* all have a match, but check just in case
    if (anyNA(api_ids)) stop(paste0("Unknown value(s) found in ", idfld))

    ## Construct a tibble for the location columns (order matters - feat_id should be first)
    loc_tbl <- tibble(feat_id = idvals,
                      loc_type = factor("aoipreset"),
                      loc_preset = factor(preset),
                      loc_fld = factor(idfld),
                      loc_qry = paste0("ref=/api/", preset, "/", api_ids, "/"))

    ## loc_qry = lapply(1:length(x$loc$val$idval), function(i)  list(ref = paste0("/api/", preset, "/", api_ids[i], "/") )))

    loc_sf <- NA
    gson_fn_base <- NA
    sf_hash <- ""

  } else if (x$loc$type == "sf") {

    ## stop("Sorry, querying by simple feature object is not yet supported")
    ## x$loc <- list(type="sf", val = list(loc = loc, idfld = idfld, idval = idval_use))
    # browser()

    idfld_name <- as.character(x$loc$val$idfld)

    loc_sf <- x$loc$val$loc %>% st_transform(4326)

    if (x$loc$val$dTolerance > 0) {
      #size_before <- object.size(loc_sf)
      loc_sf <- loc_sf %>% st_simplify(dTolerance = x$loc$val$dTolerance)
      ## Savings: (size_before - object.size(loc_sf))
    }

    ## Generate a random 5-character string to use as the base of the individual GeoJSON files
    ## We do this for each call of ca_apicalls() so that we have unique geojson file names
    ## each time data is fetched (i.e., it doesn't use an old one created by a previous run
    ## that didn't get deleted)
    gson_fn_base <- paste(sample(letters, size = 5, replace = TRUE), collapse = "")

    ## Compute the location tbl. loc_qry store the row number of each feature and will be used
    ## when fetching data.

    loc_tbl <- tibble(feat_id = x$loc$val$idval,
                      loc_type = factor("sf"),
                      loc_preset = factor(NA),
                      loc_fld = factor(idfld_name),
                      loc_qry = 1:nrow(x$loc$val$loc))

    ## We create a hash string of the sf object which will be incorporated when computing the hash for each API call.
    ## This is to differential api calls for two different SF objects that have identical id field names and values
    sf_hash <- digest(loc_sf, serialize = TRUE)


  } else if (x$loc$type == "zip") {
    stop("Sorry, querying by zip code is not yet supported")
    loc_sf <- NA
    gson_fn_base <- NA
    sf_hash <- ""


  } else {
    stop("Unknown value for location type!!")
    loc_sf <- NA
    gson_fn_base <- NA
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
                               stringsAsFactors = FALSE))

  ## If the user did not pass slug(s), construct those now
  if (identical(x$slug, NA)) {
    rs_tbl <- rs_tbl %>%
      mutate(slug = paste(cvar, period, gcm, scenario, sep="_"))
  }

  ## Add a lower case version of the slug for joining to the raster series data catalog
  rs_tbl <- rs_tbl %>% mutate(slug_lower = tolower(slug))

  ## Grab a few columns from the raster series data catalog for the slug check and units assignment
  rs_catinfo_df <- ca_catalog_rs(quiet = TRUE) %>%
     mutate(slug_lower = tolower(slug)) %>%
     select(slug_lower, rs_name = name, rs_units = units, rs_begin = begin, rs_end = end)

  ## Do the join
  rs_tbl_matches <- rs_tbl %>% left_join(rs_catinfo_df, by = "slug_lower")

  if (slug_check) {
    if (anyNA(rs_tbl_matches$rs_name)) {
      stop(paste0("Unknown slug(s): ",
                  paste(rs_tbl_matches %>% filter(is.na(rs_name)) %>% pull(slug),
                        collapse = ", ")))
    }
  }

  ## Add a column with the row numbers that we'll use later for a join
  rs_tbl <- rs_tbl %>%
    mutate(rs_idx = 1:nrow(rs_tbl)) %>%
    left_join(rs_catinfo_df, by = "slug_lower")

  ## Prep the dates. These will be the same for all API calls.
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

  ## Check the query dates againts the raster series dates
  if (date_check && !identical(x$dates, NA)) {
    rs_latest_begin_dt <- max(as.Date(substr(rs_tbl_matches$rs_begin, 1, 10), format = "%Y-%m-%d"))
    rs_earliest_end_dt <- min(as.Date(substr(rs_tbl_matches$rs_end, 1, 10), format = "%Y-%m-%d"))

    qry_begin_dt <- as.Date(format(start_dt, format = "%Y-%m-%d"))
    if (qry_begin_dt < rs_latest_begin_dt || qry_begin_dt > rs_earliest_end_dt) {
      stop("Start date beyond the range of at least one raster series")
    }

    qry_end_dt <- as.Date(format(end_dt, format = "%Y-%m-%d"))
    if (qry_end_dt < rs_latest_begin_dt || qry_end_dt > rs_earliest_end_dt) {
      stop("End date beyond the range of at least one raster series")
    }

  }

  ## Determine if the area(s) of interest are polygons (for error trapping below)
  err_msg <- "A spatial aggregation function is required to retrieve values from polygon areas. See `ca_options`."
  aoi_is_poly <- FALSE
  if (x$loc$type == "aoipreset") {
    aoi_is_poly <- TRUE
  } else if (x$loc$type == "sf") {
    if (FALSE %in% (unique(as.character(st_geometry_type(x$loc$val$loc))) %in% c("POINT", "MULTIPOINT"))) {
      aoi_is_poly <- TRUE
    }
  }

  #message(green("IS POLY = ", aoi_is_poly))

  ## Prep the spatial and temporal aggregation options
  if (identical(x$options, NA)) {

    ## Verify a spatial aggregation was passed for a polygon area
    if (aoi_is_poly) stop(err_msg)

    ## Create stat_tbl with an empty list
    stat_tbl <- tibble(stat_idx = 1, spag = "none", stat_qry = "")

  } else {

    ## x$options$spatial_ag is not NA.
    ## However we don't need to check if x$options$spatial_ag is NULL - that could only happen if
    ## someone manually edited the ca_apireq object

    spag <- x$options$spatial_ag

    ## Verify a spatial aggregation was passed for a polygon
    if (identical(spag, "none") && aoi_is_poly) stop(err_msg)

    stat_tbl <- tibble(stat_idx = 1:length(spag),
                       spag = factor(spag),
                       stat_qry = paste0("&stat=", spag))

    #stat_qry = lapply(spatial_ag, function(x) list(stat = x)))

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
  if (anyDuplicated(api_tbl$hash_int) != 0) warning(red("Duplicate values of `hash_int` found. Please file a bug report."))

    ## Generate the full URL and an integer hash of all the search components
  if (x$loc$type == "sf") {
    api_tbl <- api_tbl %>%
      mutate(api_url = sapply(paste0(ca_baseurl, "series/", slug, "/events/"), URLencode))

      # table(nchar(api_tbl2$api_hash))
      # xx <- digest::digest2int(api_tbl2$api_hash)
      # xx2 <- digest::digest2int(api_tbl2$api_hash)
      # identical(xx,xx2)

  } else {
    api_tbl <- api_tbl %>%
      mutate(api_url = sapply(paste0(ca_baseurl, "series/", slug, "/events/?", loc_qry, dt_qry, stat_qry,
                                     "&format=json"),
                              URLencode))

             # hash_int = digest2int(paste(slug, loc_type, loc_preset, loc_fld, loc_qry, start, end, spag, sep =".")))

              # api_hash = digest::digest2int(paste( slug, loc_fld, feat_id, start, end, spag,
              #                                               loc_qry, dt_qry, stat_qry)     ))

  }

  ## Prepare a list to return containing the api table and the sf object
  res <- list(api_tbl = api_tbl,
              loc_sf = loc_sf,
              idfld = idfld_name,
              gson_fn_base = gson_fn_base)

  ## Record the name of the ID field as an attribute (so it can be restored by the function
  ## that receives this input)
  #attr(res, "idfld") <- idfld_name

  ## Record the random base for geojson files as an attribute
  #attr(res, "gson_fn_base") <- gson_fn_base

  #attr(api_tbl, "idfld") <- idfld_name

  # message(silver(" - TODO: cross-check raster series period vs period"))
  # message(silver(" - TODO: cross-check location against raster series extent"))

  invisible(res)
  #invisible(api_tbl)

}

