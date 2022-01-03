#' Get cropped rasters
#'
#' Download a cropped raster for an API request
#'
#' @param x A Cal-Adapt API request
#' @param out_dir Where the output TIF files should be written
#' @param mask Mask pixels outside the location of interest with NA values
#' @param merge_geoms Whether to merge geometries, see Details
#' @param sidecar Save a small sidecar file with the TIF file containing additional attribute info
#' @param stop_on_err Stop if the server returns an error
#' @param overwrite Re-download and overwrite existing files
#' @param normalize_path Expand and normalize output file names
#' @param debug Print additional output at the console
#' @param quiet Suppress messages
#' @param write_sidecar Deprecated
#'
#' @details This will download time series cropped raster(s) for your study area, convert them to stars objects, and export them
#' as tif files. If \code{mask = TRUE}, pixels values outside the area of interest will be set to \code{NA} (\code{mask} is ignored
#' for point locations). To get a single raster per dataset that encompasses all the locations,
#' pass \code{merge_geoms = TRUE}.
#'
#' Note this will only work for areas-of-interest small enough for the Cal-Adapt API to handle (i.e., smaller than
#' San Bernadino County). If you want to download
#' rasters for a large area (e.g., the whole state of California) you're better off downloading NetCDF files from the
#' Cal-Adapt \href{http://albers.cnr.berkeley.edu/data/}{data server}.
#'
#' If \code{sidecar = TRUE}, a small file with the same base name as the tif will be saved. This sidecar file contains
#' attributes of a space-time-array not preserved by tifs. You can import the tif file back into R as a stars object
#' with \code{\link{ca_read_stars}}.
#'
#' This function merely downloads the cropped rasters to disk and returns the filenames. To work with cropped rasters
#' as stars objects within R, import them using \code{\link{ca_read_stars}}. You can also import the TIF files with other
#' packages or software.
#'
#' @return A vector of TIF file names. If \code{normalize_path = TRUE} the output file names will be expanded (absolute) and use standard slashes for the OS (see normalizePath).
#'
#' @seealso \code{\link{ca_read_stars}}
#'
#' @import crayon
#' @importFrom httr GET POST content user_agent upload_file accept_json accept http_error write_disk progress stop_for_status warn_for_status http_status
#' @importFrom utils packageVersion URLencode
#' @importFrom dplyr select mutate slice filter sym
#' @importFrom curl has_internet
#' @importFrom sf st_write st_geometry st_as_text st_bbox st_as_sfc st_sf st_geometry_type st_buffer
#' @importFrom digest digest
#' @importFrom stars read_stars write_stars st_set_dimensions
#' @importFrom stats setNames
#' @importFrom geojsonsf sf_geojson
#' @importFrom lifecycle is_present deprecate_warn deprecated
#' @importFrom zip zip_list
#'
#' @export

ca_getrst_stars <- function(x, out_dir = NULL, mask = TRUE, merge_geoms = FALSE,
                            sidecar = TRUE, stop_on_err = TRUE, overwrite = FALSE,
                            normalize_path = FALSE, debug = FALSE, quiet = FALSE,
                            write_sidecar = deprecated()) {

  ## TODO How to record units in the stars object?
  ## TODO TEST ERROR CASES - OUT OF EXTENT FOR EXAMPLE

  if (is_present(write_sidecar)) {
    deprecate_warn("0.6.0", "ca_getrst_stars(write_sidecar)", "ca_getrst_stars(sidecar)")
    sidecar <- write_sidecar
  }

  if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq", call. = FALSE)
  if (is.null(out_dir)) stop("out_dir is a required argument", call. = FALSE)
  if (!file.exists(out_dir)) stop("out_dir does not exist", call. = FALSE)

  ## Get the function(s) we'll use to style messages
  accent2 <- getOption("ca_accent2", I)
  msg_fmt <- getOption("ca_message", I)
  success_fmt <- getOption("ca_success", I)

  if (!identical(x$options, NA)) {
    if (!identical(x$options$spatial_ag, "none")) {
      msg <- "Spatial aggregtation options are ignored when downloading rasters"
      if (quiet) {
        warning(msg)
      } else {
        message(msg_fmt(msg))
      }
    }
  }

  ## Check for an internet connection
  if (!has_internet()) stop("No internet connection detected", call. = FALSE)

  ## Prepare a function process_path that will be used to process output file names (normalized, or not)
  if (normalize_path) {
    process_path <- normalizePath
  } else {
    process_path <- I
  }

  ## If we are querying an AOI preset, and either merge = TRUE or mask = FALSE,
  ## get the vector layer because we're going to need it below
  if (x$loc$type == "aoipreset" && (merge_geoms || !mask)) {
    preset_feats_sf <- ca_aoipreset_geom(x$loc$val$type, quiet = TRUE) %>%
      st_transform(4326) %>%
      select(!!sym(x$loc$val$idfld))
  }

  ## To merge geoms, we modify the location element of x
  if (merge_geoms) {

    ## If its an AOI preset, grab the features, union them, and compute the bounding box
    if (x$loc$type == "aoipreset") {

      ## Get the features so we can merge them
      feats_sf <- preset_feats_sf %>%
        filter(!!sym(x$loc$val$idfld) %in% !!x$loc$val$idval)

      if (mask) {
        ## Union all the features into one
        aoi_new_sf <- st_sf(data.frame(id = 1,
                                      geom = feats_sf %>%
                                        st_union(by_feature = FALSE)))
      } else {
        ## Create a polygon sf data frame out of the bounding box
        aoi_new_sf <- st_sf(data.frame(id = 1,
                                      geom = feats_sf %>%
                                        st_bbox() %>%
                                        st_as_sfc()))
      }

      ## Change the location value of x to the merged geom
      x <- ca_loc_sf(x, aoi_new_sf, idfld = "id", idval = NULL)


    } else if (x$loc$type == "sf") {

      ## If this is a point layer, we do not want to mask
      if (TRUE %in% (as.character(st_geometry_type(x$loc$val$loc)) %in% c("POINT", "MULTIPOINT"))) {
        mask <- FALSE
      }

      if (mask) {
        ## Union all the features into one
        aoi_new_sf <- st_sf(data.frame(id = 1,
                                  geom = x$loc$val$loc %>%
                                    st_union(by_feature = FALSE)))

      } else {
        ## Mask = FALSE, create a new sf data frame from the bounding box
        aoi_new_sf <- st_sf(data.frame(id = 1,
                                       geom = x$loc$val$loc %>%
                                         st_bbox() %>%
                                         st_as_sfc()))
      }

      ## Change the location value of x to the merged geom
      x <- ca_loc_sf(x, aoi_new_sf, idfld = "id", idval = NULL)

    } else if (x$loc$type == "pt") {

      if (nrow(x$loc$val) > 1) {

        ## Create a rectangle that encloses all points
        x_rng <- range(x$loc$val[,2])
        y_rng <- range(x$loc$val[,3])
        xs <- x_rng[c(1,2,2,1,1)]
        ys <- y_rng[c(2,2,1,1,2)]
        poly_mat <- matrix(c(xs,ys), ncol = 2)

        aoi_new_sfc <- list(poly_mat) %>%
          st_polygon(dim = "XY") %>%
          st_sfc(crs = 4326)

        aoi_new_sf <- st_sf(data.frame(id = 1, geom = aoi_new_sfc))

        ## When merging points, we don't want to mask
        ## (otherwise you can get NA values on the edges)
        mask <- FALSE

        ## Change the location value of x to the merged geom
        x <- ca_loc_sf(x, aoi_new_sf, idfld = "id", idval = NULL)

      } else {

        ## We have a single point - nothing to merge
        ## Do nothing

      }


    } else {
      stop(paste0("Unknown location type: ", x$loc$type), call. = FALSE)
    }

  } else {
    ## We are not merging features

    ## If this is a point sf data frame, set mask to TRUE
    if (x$loc$type == "sf") {
      if (TRUE %in% (as.character(st_geometry_type(x$loc$val$loc)) %in% c("POINT", "MULTIPOINT"))) {
        mask <- TRUE
      }
    }

  }

  ## If mask == FALSE, we're going to need the locagrid
  if (!mask) {
    locagrid_sf <- ca_locagrid_geom(quiet = TRUE)
  }

  ## Right here is where I would add code to tile a large AOI

  ## Get a tibble with the individual API calls
  apicalls_lst <- ca_apicalls(x, check_for = "getrst")

  idfld <- apicalls_lst$idfld

  ## Create a reduced version of the API calls including a new column for the TIF filename
  api_tbl_use <- apicalls_lst$api_tbl %>%
    select(feat_id, loc_type, loc_preset, loc_qry, start, end, cvar, period, gcm, scenario, slug, livneh, rs_name,
           rs_units, rs_begin, rs_end, tres) %>%
    mutate(tif_out_base = paste0(slug, "_", idfld, "-", feat_id))

  # Check unit consistency - taken out, not required for downloading rasters

  ## Define the user agent
  caladaptr_ua <- user_agent(paste0("caladaptr_v", packageVersion("caladaptr")))

  ## Create an object to save the results
  res <- NULL

  ## Determine if this set of API calls uses sf objects
  aoi_sf <- inherits(apicalls_lst$loc_sf, "sf")

  ## Loop through calls
  if (debug) message(msg_fmt(paste0(" - going to make ", nrow(api_tbl_use), " api calls")))

  for (i in 1:nrow(api_tbl_use)) {

    ## Construct the output file name. File names should be unique with the exception of date ranges
    ## i.e., the start and end dates are not encoded in the file name, but everything else is
    tif_fn_out <- file.path(out_dir, paste0(api_tbl_use[i, "tif_out_base", drop = TRUE], ".tif"))

    ## See if this TIF file has already been downloaded
    if (file.exists(tif_fn_out) && !overwrite) {
      if (!quiet) message(msg_fmt(paste0(" - ", tif_fn_out, " found. Skipping.")))

      ## Add the file name to the result anyway
      res <- c(res, process_path(tif_fn_out))

    } else {

      if (!quiet) message(accent2(paste0(" - requesting: ", api_tbl_use[i, "rs_name", drop = TRUE], "\n   for ", idfld, ": ",
                                       api_tbl_use[i, "feat_id", drop = TRUE])))

      zip_fn <- tempfile(fileext = ".zip")
      feat_id <- api_tbl_use[i, 1, drop = TRUE]
      dt_start <- as.Date(api_tbl_use[i, "start", drop = TRUE])   ## could be NA
      dt_end <- as.Date(api_tbl_use[i, "end", drop = TRUE])       ## could be NA
      tres <- api_tbl_use[i, "tres", drop = TRUE]
      slug <- api_tbl_use[i, "slug", drop = TRUE]

      ## Create a list containing metadata about this dataset. This will later
      ## be saved as an attribute of the stars object, and saved to disk in the sidecar

      data_metadata_lst <- list(cvar = api_tbl_use$cvar[i] %>% as.character(),
                                units = api_tbl_use$rs_units[i] %>% as.character(),
                                scenario = api_tbl_use$scenario[i] %>% as.character(),
                                gcm = api_tbl_use$gcm[i] %>% as.character(),
                                period = api_tbl_use$period[i] %>% as.character(),
                                slug = api_tbl_use$slug[i] %>% as.character(),
                                livneh = api_tbl_use$livneh[i],
                                start = api_tbl_use$start[i] %>% as.character(),
                                end = api_tbl_use$end[i] %>% as.character(),
                                idfld = idfld,
                                idval = feat_id
                                )

      ## Can we use date slicing syntax (to filter by date on the server)?
      use_date_slice <- !is.na(dt_start) &&
        (tres == "annual" || tres == "monthly") &&
        !identical(getOption("ca_date_slice", NA), FALSE)
      if (debug) message(msg_fmt(paste0(" - date slicing with URL: ", use_date_slice)))

      if (use_date_slice) {
        rast_or_dates <- paste0(api_tbl_use[i, "start", drop = TRUE], "/",
                                api_tbl_use[i, "end", drop = TRUE])
      } else {
        rast_or_dates <- "rasters"
      }

      ## If the AOI is a sf data frame, we have to use a POST request
      if (aoi_sf) {

        this_feat_sf <- apicalls_lst$loc_sf %>%
          slice(api_tbl_use[i, "loc_qry", drop = TRUE])

        if (mask) {
          ## Generate the geojson text to pass in the POST request
          geojson_chr <- this_feat_sf %>%
            sf_geojson(atomise = TRUE)

        } else {    ## NO MASK

          ## The Cal-Adapt API returns all grid cells that intersect the polygon,
          ## but pixels on the edges will have NA values unless the feature
          ## overlaps the grid center. To prevent this, instead of passing the
          ## polygon, we find the actual loca grid cells that intersect, and
          ## send the bounding box of those (with a small internal buffer to
          ## prevent extra pixels on the edges)

          ## Get the locagrids that intersect this feature
          suppressMessages(
            locagrid_intersects_sf <- locagrid_sf[this_feat_sf, , drop = FALSE]
          )

          ## Get the bounding box of the intersecting locagrids, and take
          ## a small internal buffer (to avoid getting extra pixels on the outside)
          suppressMessages(
            suppressWarnings(
              locagrids_bb_sfc <- locagrid_intersects_sf %>%
                st_bbox() %>%
                st_as_sfc() %>%
                st_buffer(-0.001)
            )
          )

          ## Turn this rectangle sfc into a sf data frame
          ## (we *do* need an attribute table)
          onepoly_bb_sf = st_sf(data.frame(id = 1, geom = locagrids_bb_sfc))

          ## Generate the geojson text
          geojson_chr <- sf_geojson(onepoly_bb_sf, atomise = TRUE)

        }

        ## Create the body list
        ## All we need in the body is the geometry. stat is not needed for rasters
        ## If dates were specified for annual date, we'll put them in the URL, not in the body.
        ## Note also we're using the 'g' parameter instead of the 'features' parameter
        ## because the API doesn't support the features parameter when requesting a TIF
        body_flds_lst <- list(g = geojson_chr)

        ## Construct the POST URL
        post_url <- paste0(ca_baseurl, "series/", api_tbl_use[i, "slug", drop = TRUE],
                               "/", rast_or_dates, "/")

        if (debug) {
          message(msg_fmt(paste0(" - (", i, "/", nrow(api_tbl_use), ") PUT ", post_url, ". (", idfld, "=",
                                         feat_id, ", start='", dt_start, "', end='", dt_end, "')")))
        }

        ## Make the POST request
        if (quiet) {
          qry_resp <- POST(url = post_url, body = body_flds_lst, encode = "multipart",
                           accept("application/zip"), caladaptr_ua)
        } else {
          qry_resp <- POST(url = post_url, body = body_flds_lst, encode = "multipart",
                           accept("application/zip"), caladaptr_ua, progress())
        }

      } else if (api_tbl_use[i, "loc_type", drop = TRUE] == "aoipreset" && !mask) {

        ## This is an AOI preset but unmasked. Hence we need to pass the bounding box
        ## of this feature as a sf object

        ## Create a sfc object from the bounding box
        onepoly_bb_sfc <- preset_feats_sf %>%
          filter(!!sym(idfld) == !!api_tbl_use[i, "feat_id", drop = TRUE]) %>%
          st_bbox() %>%
          st_as_sfc()

        ## Get the locagrids that intersect this feature
        suppressMessages(
          locagrid_intersects_sf <- locagrid_sf[onepoly_bb_sfc, , drop = FALSE]
        )

        ## Get the bounding box of the intersecting locagrids, and take
        ## a small internal buffer (to avoid getting extra pixels on the outside)
        suppressMessages(
          suppressWarnings(
            locagrids_bb_sfc <- locagrid_intersects_sf %>%
              st_bbox() %>%
              st_as_sfc() %>%
              st_buffer(-0.001)
          )
        )

        ## Turn it into a sf data frame
        onepoly_bb_sf = st_sf(data.frame(geom = locagrids_bb_sfc))
        onepoly_bb_sf[[idfld]] <- api_tbl_use[i, "feat_id", drop = TRUE]

        ## Generate the geojson text
        geojson_chr <- sf_geojson(onepoly_bb_sf, atomise = TRUE)

        ## Create the body list
        body_flds_lst <- list(g = geojson_chr)

        ## Construct the POST URL
        post_url <- paste0(ca_baseurl, "series/", api_tbl_use[i, "slug", drop = TRUE],
                           "/", rast_or_dates, "/")

        if (debug) {
          message(msg_fmt(paste0(" - (", i, "/", nrow(api_tbl_use), ") PUT ", post_url, ". (", idfld, "=",
                                feat_id, ", start='", dt_start, "', end='", dt_end, "')")))
        }

        ## Make the POST request
        if (quiet) {
          qry_resp <- POST(url = post_url, body = body_flds_lst, encode = "multipart", accept("application/zip"), caladaptr_ua)
        } else {
          qry_resp <- POST(url = post_url, body = body_flds_lst, encode = "multipart", accept("application/zip"), caladaptr_ua, progress())
        }

      } else if (api_tbl_use[i, "loc_type", drop = TRUE] == "pt" ||
                 api_tbl_use[i, "loc_type", drop = TRUE] == "aoipreset" && mask) {

        ## Either loc_type == "pt", or AOI Preset (with mask). We can use a GET request

        ## Querying using the ref=___ URL will return a masked TIF
        ## We can't use

        api_url_full <- paste0(ca_baseurl, "series/", api_tbl_use[i, "slug", drop = TRUE],
                               "/", rast_or_dates, "/?",
                               api_tbl_use[i, "loc_qry", drop = TRUE]) %>% URLencode()

        if (debug) message(msg_fmt(paste0(" - (", i, "/", nrow(api_tbl_use), ") ", api_url_full)))

        if (quiet) {
          qry_resp <- GET(api_url_full, accept("application/zip"), caladaptr_ua)
        } else {
          qry_resp <- GET(api_url_full, accept("application/zip"), caladaptr_ua, progress())
        }

        # qry_resp <- GET(utils::URLencode(api_url_full), caladaptr_ua)

      } else {
        stop("Don't know how to handle this location type", call. = FALSE)

      }

      ## At this point we have a response object

      ## See if the server sent an error
      if (http_error(qry_resp)) {
        if (stop_on_err) {
          stop_for_status(qry_resp)

        } else {

          ## Don't stop - print a message or generate a warning
          if (quiet && !debug) {
            warn_for_status(qry_resp)

          } else {
            ## quiet = FALSE or debug = TRUE
            message(red(paste0(" - Oh dear. ", http_status(qry_resp)$message)))
          }

        }


      } else {

        # Good response (hopefully a zip file). Write it to a temp file.
        writeBin(content(qry_resp, type = "application/zip"), zip_fn)

        ## Did it work?
        if (file.exists(zip_fn)) {

          ## Are there actually tifs in the zip file?
          if (nrow(zip_list(zip_fn)) > 0) {

            ## Construct a name for the layer
            layer_name <- paste0(slug, "_", idfld, "-", feat_id)

            ## Process tif(s) depending on whether we used date slicing or not.
            if (use_date_slice) {

              ## Yearly and monthly rasters come down as individual tifs
              tifs_unzipped <- unzip(zip_fn, exdir = tempdir(), junkpaths = TRUE)

              ## Because we used date slicing, the TIFs have already been date filtered.
              ## But we have to define the "along list" which we'll use when converting to stars
              ## to set the temporal dimension.

              ## We can assume that dt_start and dt_end have values (otherwise we wouldn't be using a date slicing URL)

              if (tres == "annual") {
                year1 <- as.POSIXlt(dt_start)$year + 1900
                year2 <- as.POSIXlt(dt_end)$year + 1900
                along_lst <- list(year = year1:year2)

              } else {
                if (as.POSIXlt(dt_start)$mday != 1) {
                  warning("When getting a monthly dataset, the start date should be the first of the month")
                }
                ## Construct a vector of POSIXct values for the first day of every month in the date range
                along_lst <- list(month = seq(dt_start, dt_end, by = "month"))
              }

              ## Read the tifs as a stars object
              my_stars <- read_stars(tifs_unzipped, along = along_lst) %>% setNames(layer_name)

              ## Write the stars objects as TIF
              ## tif_fn_out <- file.path(out_dir, paste0(api_tbl_use[i, "tif_out_base", drop = TRUE], ".tif")) DEFINED ABOVE
              write_stars(my_stars, dsn = tif_fn_out, layer = names(my_stars)[1])

              if (sidecar) {

                ## Generate the location metadata
                loc_metadata_lst <- list(rows = nrow(my_stars) %>% as.numeric(),
                                         cols = ncol(my_stars) %>% as.numeric())

                ## Combine the data and location metadata lists, and save them as an attribute of my_stars
                attr(my_stars, "ca_metadata") <- c(data_metadata_lst, loc_metadata_lst)

                ## Write the attribute data as a sidecar
                sidecar_fn_out <- file.path(out_dir, paste0(api_tbl_use[i, "tif_out_base", drop = TRUE], ".attr.rds"))

                saveRDS(attributes(my_stars)[c("names", "dimensions", "ca_metadata")], file = sidecar_fn_out)
              }

              res <- c(res, process_path(tif_fn_out))
              unlink(tifs_unzipped)

            } else {

              ## There was no date slicing. If this is daily data, then it is probably a single tif with thousands of layers.
              ## If it is monthly or annual data, then it should be one single-band tif per date point

              ## Get a list of tifs in the zip file
              tifs_in_zip <- unzip(zip_fn, list = TRUE)$Name

              ## Get the begin and end dates for the entire raster series
              rs_begin_dt <- api_tbl_use[i, "rs_begin", drop = TRUE] %>% substring(1,10) %>% as.Date()
              rs_end_dt <- api_tbl_use[i, "rs_end", drop = TRUE] %>% substring(1,10) %>% as.Date()

              if (tres == "daily") {

                if (length(tifs_in_zip) == 1) {   ## this is expected - a single tif with one layer for each day

                  ## Unzip the single tif file
                  tif_unzipped <- unzip(zip_fn, exdir = tempdir(), junkpaths = TRUE)

                  ## Create a stars raster (can not use along argument, because there is only one TIF coming in)
                  my_stars <- read_stars(tif_unzipped) %>% setNames(layer_name)

                  ## Create a sequence of daily dates for the entire raster series
                  days_dates <- seq(rs_begin_dt, rs_end_dt, by = "day")

                  if (length(days_dates) == dim(my_stars)[3]) {

                    ## We're in good shape. Each layer represents a date

                    ##############################################################################
                    ## We need to adjust the 'band' dimension, giving it a new name and values.
                    ## When all is said and done, it should look like the following:
                    ## from   to     offset   delta refsys point values
                    ## x       1    2     -121.5  0.0625 WGS 84 FALSE   NULL [x]
                    ## y       1    2       40.5 -0.0625 WGS 84 FALSE   NULL [y]
                    ## date    1 1827 2070-01-01  1 days   Date    NA   NULL
                    ##############################################################################

                    my_stars <- st_set_dimensions(my_stars, which = "band", names = "date", values = days_dates)

                    if (FALSE) {
                      ## THIS WAS AN EFFORT TO PREENT THE STRANGE ERROR MESSAGE WITH FILTER. IT TURNED OUT
                      ## TO NOT BE NECESSARY BECAUSE THAT ERROR MESSAGE HAD NOTHING TO DO WITH THE DATES
                      my_stars <- st_set_dimensions(my_stars, names = c("x", "y", "date"))
                      ## We have to modify the values of the third dimension
                      d3 <- attributes(my_stars)$dimensions[[3]]
                      d3$values <- days_dates
                      d3$refsys <- "Date"
                      attributes(my_stars)$dimensions[[3]] <- d3
                    }

                    ## Filter by date if needed
                    if (!is.na(dt_start)) {

                      if (1 %in% dim(my_stars)[1:2]) {
                        ## If the x or y dimension are 1, the filter() function generates an error:
                        ## "Error in set_dimension_values "cannot derive cell boundaries from a single center: specify start and end"
                        ## So instead we use square bracket notation. This works but it doesn't update the
                        ## the offset value so is harder to read
                        keep_idx <- which(days_dates >= dt_start & days_dates <= dt_end)
                        my_stars <- my_stars[ , , , keep_idx]
                        if (!quiet) message(red(" - note: this tif has a single row or single column. Tidyverse verbs like filter() and slice() may not work on it (use square brackets instead)"))

                      } else {
                        ## Filter by date
                        my_stars <- my_stars %>%
                          filter(date >= dt_start) %>%
                          filter(date <= dt_end)
                      }

                    }

                    if (!quiet) message(accent2(" - converting rasters to stars"))

                    ## Write my_stars to disk as TIF
                    ## tif_fn_out <- file.path(out_dir, paste0(api_tbl_use[i, "tif_out_base", drop = TRUE], ".tif"))  DEFINED ABOVE
                    if (!quiet) message(accent2(paste0(" - saving to ", tif_fn_out)))
                    write_stars(my_stars, dsn = tif_fn_out, layer = names(my_stars)[1])

                    if (sidecar) {
                      ## Generate the location metadata
                      loc_metadata_lst <- list(rows = nrow(my_stars) %>% as.numeric(),
                                               cols = ncol(my_stars) %>% as.numeric())

                      ## Combine the data and location metadata lists, and save them as an attribute of my_stars
                      attr(my_stars, "ca_metadata") <- c(data_metadata_lst, loc_metadata_lst)

                      ## Write the attribute data as a sidecar
                      sidecar_fn_out <- file.path(out_dir, paste0(api_tbl_use[i, "tif_out_base", drop = TRUE], ".attr.rds"))
                      saveRDS(attributes(my_stars)[c("names", "dimensions", "ca_metadata")], file = sidecar_fn_out)
                    }

                    res <- c(res, process_path(tif_fn_out))

                  } else {
                    warning("NUmber of layers in the tif does not match the expected number of time steps. Skipping.")
                  }

                  ## Done with the tif
                  unlink(tif_unzipped)

                } else {
                  stop(paste0("Expected one tif and received ", length(tifs_in_zip), ". Don't know how to proceed."))
                }

              } else if (tres == "monthly") {

                ## Need to figure out 1) which single tifs to unzip, and 2) compute values for the dimensions

                ## First construct a vector of POSIXct values for the first day of every month in the raster series.
                month_dates <- seq(rs_begin_dt, rs_end_dt, by = "month")

                if (length(tifs_in_zip) == length(month_dates)) {
                  ## We're in good shape. The expected number of TIF files was returned. We
                  ## can assume that each tif is for a single month

                  if (is.na(dt_start)) {
                    keep_idx <- 1:length(tifs_in_zip)
                  } else {
                    keep_idx <- which(month_dates >= dt_start & month_dates <= dt_end)
                  }

                  if (!quiet) message(accent2(" - converting rasters to stars"))

                  ## Yearly data come down as individual tifs
                  tifs_unzipped <- unzip(zip_fn, files = tifs_in_zip[keep_idx],
                                         exdir = tempdir(), junkpaths = TRUE)

                  along_lst <- list(date = month_dates[keep_idx])

                  my_stars <- read_stars(tifs_unzipped, along = along_lst) %>% setNames(layer_name)

                  ## Write the stars objects as TIF
                  ## tif_fn_out <- file.path(out_dir, paste0(api_tbl_use[i, "tif_out_base", drop = TRUE], ".tif")) DEFINED ABOVE
                  if (!quiet) message(accent2(paste0(" - saving to ", tif_fn_out)))
                  write_stars(my_stars, dsn = tif_fn_out, layer = names(my_stars)[1])

                  if (sidecar) {
                    ## Generate the location metadata
                    loc_metadata_lst <- list(rows = nrow(my_stars) %>% as.numeric(),
                                             cols = ncol(my_stars) %>% as.numeric())

                    ## Combine the data and location metadata lists, and save them as an attribute of my_stars
                    attr(my_stars, "ca_metadata") <- c(data_metadata_lst, loc_metadata_lst)

                    ## Write the attribute data as a sidecar
                    sidecar_fn_out <- file.path(out_dir, paste0(api_tbl_use[i, "tif_out_base", drop = TRUE], ".attr.rds"))
                    saveRDS(attributes(my_stars)[c("names", "dimensions", "ca_metadata")], file = sidecar_fn_out)
                  }

                  res <- c(res, process_path(tif_fn_out))
                  unlink(tifs_unzipped)

                } else {
                  if (!quiet) message(red(paste0(" - unexpected number of TIF files, can't convert to stars")))
                }

              } else if (tres == "annual") {

                ## No date slicing + annual data probably means the API request didn't have a start and end date

                year_dates <- seq(rs_begin_dt, rs_end_dt, by = "year")

                if (length(tifs_in_zip) == length(year_dates)) {
                  ## We're in good shape. The expected number of TIF files was returned.

                  if (is.na(dt_start)) {
                    keep_idx <- 1:length(tifs_in_zip)
                  } else {
                    keep_idx <- which(year_dates >= dt_start & year_dates <= dt_end)
                  }

                  if (!quiet) message(accent2(" - converting rasters to stars"))

                  ## Yearly data come down as individual tifs
                  tifs_unzipped <- unzip(zip_fn, files = tifs_in_zip[keep_idx],
                                         exdir = tempdir(), junkpaths = TRUE)

                  year_nums <- as.POSIXlt(year_dates)$year + 1900
                  along_lst <- list(year = year_nums[keep_idx])

                  my_stars <- read_stars(tifs_unzipped, along = along_lst) %>% setNames(layer_name)

                  ## Write the stars objects as TIF
                  ## tif_fn_out <- file.path(out_dir, paste0(api_tbl_use[i, "tif_out_base", drop = TRUE], ".tif")) DEFINED ABOVE
                  if (!quiet) message(accent2(paste0(" - saving to ", tif_fn_out)))
                  write_stars(my_stars, dsn = tif_fn_out, layer = names(my_stars)[1])

                  if (sidecar) {
                    ## Generate the location metadata
                    loc_metadata_lst <- list(rows = nrow(my_stars) %>% as.numeric(),
                                             cols = ncol(my_stars) %>% as.numeric())

                    ## Combine the data and location metadata lists, and save them as an attribute of my_stars
                    attr(my_stars, "ca_metadata") <- c(data_metadata_lst, loc_metadata_lst)

                    ## Write the attribute data as a sidecar
                    sidecar_fn_out <- file.path(out_dir, paste0(api_tbl_use[i, "tif_out_base", drop = TRUE], ".attr.rds"))
                    saveRDS(attributes(my_stars)[c("names", "dimensions", "ca_metadata")], file = sidecar_fn_out)
                  }

                  res <- c(res, process_path(tif_fn_out))
                  unlink(tifs_unzipped)

                } else {
                  stop("Unepxected number of TIFs returned. Please contact the package author for help.")
                }

              }


            }

            ## We're done with the zip file
            unlink(zip_fn)


          } else {   ##  if nrow(zip::zip_list(zip_fn)) > 0

            if (debug) message(red(" - empty zip file. This is not supposed to happen. Please contact the package author or create a GitHub issue."))

            ## We're done with the zip file
            unlink(zip_fn)

          }


        } else {
          ## Nothing returned - maybe the date fell outside of range? location outside extent?
          if (debug) message(red(" - no zip file returned from server. This is not supposed to happen. Please contact the package author or create a GitHub issue."))
        }

      }

    }




  } ## for (i in 1:nrow(api_tbl_use)) {

  if (!quiet && length(res) > 0) {
    message(success_fmt(" - Done. Read TIFs in with `ca_stars_read`"))
  }

  invisible(res)

}

