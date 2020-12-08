#' Get a cropped raster
#'
#' Download a cropped raster for an API request
#'
#' @param x A Cal-Adapt API request
#' @param out_dir Where the output tif files should be written
#' @param merge_geoms Whether to merge geoms (not currently used)
#' @param sidecar_write Save a small sidecar file with the TIF file containing additional attribute info
#' @param stop_on_err Stop if the server returns an error
#' @param quiet Suppress messages
#' @param debug Print additional output at the console
#'
#' @details This will download cropped multi-layer raster(s) for your study area, convert them to stars objects, and export them
#' as tif files. Note this will only work for areas-of-interest small enough for the Cal-Adapt to handle (i.e., smaller than
#' San Bernadino County). If you want to download
#' rasters for a large area (e.g., the whole state of California) you're better off downloading NetCDF files from the
#' Cal-Adapt \href{http://albers.cnr.berkeley.edu/data/}{data server}.
#'
#' If \code{sidecar_write = TRUE}, a small file with the same base name as the tif will be saved. This sidecar file contains
#' attributes of a space-time-array not preserved by tifs. You can import the tif file back into R as a stars object
#' with \code{\link{ca_read_stars}}.
#'
#' This function merely downloads the cropped rasters to disk and returns the filenames. To work with cropped rasters
#' as stars objects within R, import them using \code{\link{ca_read_stars}}.
#'
#' @return A vector of TIF file names
#'
#' @import crayon
#' @importFrom httr GET POST content user_agent upload_file accept_json accept http_error write_disk progress stop_for_status warn_for_status
#' @importFrom utils packageVersion
#' @importFrom units set_units
#' @importFrom dplyr select mutate slice filter
#' @importFrom curl has_internet
#' @importFrom sf st_write st_geometry st_as_text
#' @importFrom digest digest
#' @importFrom stars read_stars write_stars st_set_dimensions
#' @importFrom stats setNames
#' @export

ca_getrst_stars <- function(x, out_dir = NULL, merge_geoms = TRUE,
                            sidecar_write = TRUE, stop_on_err = TRUE,
                            quiet = FALSE, debug = FALSE) {

  ## TODO How to save units in the dimensions table?
  ## TEST ERROR CASES - OUT OF EXTENT FOR EXAMPLE

  if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq")
  if (x$loc$type == "pt") stop("Sorry, you can't download rasters for point query")
  if (is.null(out_dir)) stop("out_dir is a required argument")
  if (!file.exists(out_dir)) stop("out_dir does not exist")

  accent2 <- getOption("ca_accent2", paste0)

  if (!identical(x$options, NA)) {
    if (!identical(x$options$spatial_ag, "none")) {
      msg <- "Spatial aggregtation options are ignored when downloading rasters"
      if (quiet) {
        warning(msg)
      } else {
        message(accent2(msg))
      }
    }
  }

  ## Check for an internet connection
  if (!has_internet()) stop("No internet connection detected")

  ## Get a tibble with the individual API calls
  apicalls_lst <- ca_apicalls(x, ignore_spag = TRUE)
  api_tbl <- apicalls_lst$api_tbl
  idfld <- apicalls_lst$idfld

  ## Create a reduced version of the API calls
  api_tbl_use <- api_tbl %>%
    select(feat_id, loc_type, loc_preset, loc_qry, period, start, end, slug, rs_name,
           rs_units, rs_begin, rs_end, tres) %>%
    mutate(tif_out_base = paste0(slug, "_", idfld, feat_id))

  ## Get the cache dir to save temporary geojson files
  ## (we save them here rather than tempdir so httptest will
  ## generate the same hash for the API call each time)
  ca_cache_dir <- ca_getcache()

  if (length(unique(api_tbl_use$rs_units)) > 1) {
    # stop("Can not process this API request: Raster series have different units")
  }

  ## Define the user agent
  caladaptr_ua <- user_agent(paste0("caladaptr_v", packageVersion("caladaptr")))

  ## Create an object to save the results
  res <- NULL

  ## Determine if this set of API calls uses sf objects
  aoi_sf <- inherits(apicalls_lst$loc_sf, "sf")

  ## Compute the file names which will be used to export individual features to temporary geojson files
  ## There should be one file name for each feature (row)
  if (aoi_sf) {

    stop("Sorry, downloading data via a sf object is not yet working")

    ## THIS IS WHERE I COULD MERGE THE GEOMS

    features_hashes <- unname(sapply(apicalls_lst$loc_sf %>%
                                st_geometry() %>% st_as_text(),
                              digest,
                              algo = "crc32",
                              file = FALSE))

    gjsn_all_fn <- file.path(ca_cache_dir,
                             paste0("~ca_", features_hashes, ".geojson"))

    if (anyDuplicated(gjsn_all_fn) > 0) warning(red("Duplicate feature hashes(s) encountered"))

  }

  ## Loop through calls
  if (debug) message(silver(paste0(" - going to make ", nrow(api_tbl_use), " api calls")))

  for (i in 1:nrow(api_tbl_use)) {

    if (!quiet) message(accent2(paste0(" - requesting: ", api_tbl_use[i, "rs_name", drop = TRUE], "\n   for ", idfld, ": ",
                                     api_tbl_use[i, "feat_id", drop = TRUE])))

    zip_fn <- tempfile(fileext = ".zip")
    feat_id <- api_tbl_use[i, 1, drop = TRUE]
    dt_start <- as.Date(api_tbl_use[i, "start", drop = TRUE])   ## could be NA
    dt_end <- as.Date(api_tbl_use[i, "end", drop = TRUE])       ## could be NA
    tres <- api_tbl_use[i, "tres", drop = TRUE]
    slug <- api_tbl_use[i, "slug", drop = TRUE]

    use_date_slice <- !is.na(dt_start) && (tres == "annual")

    if (use_date_slice) {
      rast_or_dates <- paste0(api_tbl_use[i, "start", drop = TRUE], "/",
                              api_tbl_use[i, "end", drop = TRUE])
    } else {
      rast_or_dates <- "rasters"
    }


    if (aoi_sf) {

      #browser()

      ## Get the temporary geojson file name for this row (which is stored in the loc_qry column)
      gjsn_fn <- gjsn_all_fn[api_tbl_use[i, "loc_qry", drop = TRUE]]

      if (!file.exists(gjsn_fn)) {
        if (debug) message(silver(paste0(" - saving temp geojson: ", basename(gjsn_fn))))
        st_write(apicalls_lst$loc_sf %>%
                   slice(api_tbl_use[i, "loc_qry", drop = TRUE]),
                 dsn = gjsn_fn,
                 quiet = TRUE)
      }

      body_flds_lst <- list(features = upload_file(gjsn_fn))

      post_url <- paste0(ca_baseurl, "series/", api_tbl_use[i, "slug", drop = TRUE],
                             "/", rast_or_dates, "/")

      if (debug) {
        message(silver(paste0(" - (", i, "/", nrow(api_tbl_use), ") PUT ", post_url, ". (", idfld, "=",
                                       feat_id, ", start='", dt_start, "', end='", dt_end, "')")))
      }

      ## Make the POST request
      # browser()
      # zip_fn <- "c:/temp/pinnacles02.zip"
      qry_resp <- POST(url = post_url,
                       body = body_flds_lst,
                       encode = "multipart",
                       accept("application/zip"),
                       caladaptr_ua, progress())

      ## Troubleshoot direction #1: Verify there are some values to return using a traditional json query
      ## The result that comes back is not clipped to the features

      if (FALSE) {
        qry_resp <- POST(url = post_url,
                         body = body_flds_lst,
                         body = c(body_flds_lst, list(stat = "mean")),
                         encode = "multipart",
                         accept_json(),
                         httr::verbose(),
                         caladaptr_ua, progress())
        qry_resp$status_code

        ## Good response. Convert content to a list
        # qry_content <- content(qry_resp, type = "application/json")
        # qry_content$count #95
        # qry_content$results
        # these_vals <- unlist(qry_content$data)
        # these_vals
      }

    } else {   ## SEND A GET REQUEST

      api_url_full <- paste0(ca_baseurl, "series/", api_tbl_use[i, "slug", drop = TRUE],
                             "/", rast_or_dates, "/?",
                             api_tbl_use[i, "loc_qry", drop = TRUE])

      if (debug) message(silver(paste0(" - (", i, "/", nrow(api_tbl_use), ") ", api_url_full)))

      qry_resp <- GET(api_url_full, accept("application/zip"), caladaptr_ua, progress())

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

      # Good response
      ## Convert response to raw
      qry_content_raw <- content(qry_resp, type = "application/zip")
      writeBin(qry_content_raw, zip_fn)

      ## No server error
      if (file.exists(zip_fn)) {

        ## Right here I need to check if the zip file is empty (which is what will happen if no returns were returned)
        # browser()

        layer_name <- paste0(slug, "_", idfld, feat_id)

        ## Got a zip file
        if (use_date_slice) {

          ## Yearly data come down as individual tifs
          tifs_unzipped <- unzip(zip_fn, exdir = tempdir(), junkpaths = TRUE)

          ## The TIFs have already been time filtered.
          ## Define the along list which is used to set the temporal dimension
          if (tres == "annual") {
            year1 <- as.POSIXlt(dt_start)$year + 1900
            year2 <- as.POSIXlt(dt_end)$year + 1900
            along_lst <- list(year = year1:year2)

          } else {
            stop(paste0("Sorry, ", tres, " is not yet supported for date slicing. Please email the package author or start a GitHub issue to have it added"))
          }

          my_stars <- read_stars(tifs_unzipped, along = along_lst) %>% setNames(layer_name)

          ## Write the stars objects as TIF
          tif_fn_out <- file.path(out_dir, paste0(api_tbl_use[i, "tif_out_base", drop = TRUE], ".tif"))
          write_stars(my_stars, dsn = tif_fn_out, layer = names(my_stars)[1])

          ## Write the attribute data as a sidecar
          sidecar_fn_out <- file.path(out_dir, paste0(api_tbl_use[i, "tif_out_base", drop = TRUE], ".attr.rds"))
          saveRDS(attributes(my_stars)[c("names", "dimensions")], file = sidecar_fn_out)

          res <- c(res, tif_fn_out)
          unlink(tifs_unzipped)

        } else {

          ## There was no date slicing. If this is daily data, then it is probably a single tif with thousands of layers.
          ## If it is month or annual data, then it should be one single-band tif per date point

          ## Gat a list of tifs in the zip file
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
                    if (!quiet) message(" - this tif has a single row or single column, which may prevent the filter() function from working")

                  } else {
                    ## Filter by date
                    my_stars <- my_stars %>%
                      filter(date >= dt_start) %>%
                      filter(date <= dt_end)
                  }

                }

                if (!quiet) message(accent2(" - converting rasters to stars"))

                ## Write the stars object to disk as TIF
                tif_fn_out <- file.path(out_dir, paste0(api_tbl_use[i, "tif_out_base", drop = TRUE], ".tif"))
                if (!quiet) message(accent2(paste0(" - saving to ", tif_fn_out)))
                write_stars(my_stars, dsn = tif_fn_out, layer = names(my_stars)[1])

                if (sidecar_write) {
                  ## Write the attribute data as a sidecar
                  sidecar_fn_out <- file.path(out_dir, paste0(api_tbl_use[i, "tif_out_base", drop = TRUE], ".attr.rds"))
                  saveRDS(attributes(my_stars)[c("names", "dimensions")], file = sidecar_fn_out)
                }

                res <- c(res, tif_fn_out)

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
              tif_fn_out <- file.path(out_dir, paste0(api_tbl_use[i, "tif_out_base", drop = TRUE], ".tif"))
              if (!quiet) message(accent2(paste0(" - saving to ", tif_fn_out)))
              write_stars(my_stars, dsn = tif_fn_out, layer = names(my_stars)[1])

              if (sidecar_write) {
                ## Write the attribute data as a sidecar
                sidecar_fn_out <- file.path(out_dir, paste0(api_tbl_use[i, "tif_out_base", drop = TRUE], ".attr.rds"))
                saveRDS(attributes(my_stars)[c("names", "dimensions")], file = sidecar_fn_out)
              }

              res <- c(res, tif_fn_out)
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
              tif_fn_out <- file.path(out_dir, paste0(api_tbl_use[i, "tif_out_base", drop = TRUE], ".tif"))
              if (!quiet) message(accent2(paste0(" - saving to ", tif_fn_out)))
              write_stars(my_stars, dsn = tif_fn_out, layer = names(my_stars)[1])

              if (sidecar_write) {
                ## Write the attribute data as a sidecar
                sidecar_fn_out <- file.path(out_dir, paste0(api_tbl_use[i, "tif_out_base", drop = TRUE], ".attr.rds"))
                saveRDS(attributes(my_stars)[c("names", "dimensions")], file = sidecar_fn_out)
              }

              res <- c(res, tif_fn_out)
              unlink(tifs_unzipped)

            } else {
              stop("Unepxected")
            }

          }


        }

        ## We're done with the zip file
        unlink(zip_fn)

      } else {
        ## Nothing returned - date fell outside of range? location outside extent?
        if (debug) message(silver(" - no raster returned"))
      }

    }

  } ## for (i in 1:nrow(api_tbl)) {

  ## Delete any temporary geojson files created (including this run and previous runs)
  if (aoi_sf) {
    tmp_jsn_fn <- list.files(ca_cache_dir, pattern = "^\\~ca_(.*).geojson$", full.names = TRUE)
    if (length(tmp_jsn_fn) > 0) {
      if (debug) message(silver(paste0(" - deleting ", length(tmp_jsn_fn), " temp geojson files")))
      unlink(tmp_jsn_fn)
    }
  }

  invisible(res)

}

