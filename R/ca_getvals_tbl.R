#' Get values from an API request object as a tibble
#'
#' @param x A Cal-Adapt API request
#' @param quiet Suppress messages
#' @param debug Print additional output at the console
#' @param stop_on_err Stop if the server returns an error
#'
#' @return A tibble
#'
#' @import crayon
#' @importFrom httr GET POST content modify_url user_agent upload_file accept_json http_error stop_for_status warn_for_status
#' @importFrom utils txtProgressBar setTxtProgressBar packageVersion
#' @importFrom units set_units
#' @importFrom dplyr select mutate left_join slice
#' @importFrom curl has_internet
#' @importFrom sf st_write st_geometry st_as_text
#' @importFrom digest digest
#' @export

ca_getvals_tbl <- function(x, quiet = FALSE, debug = FALSE, stop_on_err = TRUE) {

  if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq")

  ## Check for an internet connection
  if (!has_internet()) stop("No internet connection detected")

  accent2 <- getOption("ca_accent2", paste0)

  ## Get a tibble with the individual API calls
  apicalls_lst <- ca_apicalls(x)
  api_tbl <- apicalls_lst$api_tbl

  ## Get the name of first column
  feat_id_fldname <- apicalls_lst$idfld

  ## Get the cache dir to save temporary geojson files
  ## (we save them here rather than tempdir so httptest will
  ## generate the same hash for the API call each time)
  ca_cache_dir <- ca_getcache()

  ## Rename the first column (feature id)
  names(api_tbl)[1] <- feat_id_fldname

  if (length(unique(api_tbl$rs_units)) > 1) {
    stop("Can not process this API request: Raster series have different units")
  }

  ## Define which columns to save in the output table (either in-memory tibble or db)
  if (identical(x$slug, NA)) {
    apicall_cols_keep <- c(names(api_tbl)[1], "cvar", "period", "gcm", "scenario", "spag")

  } else {
    apicall_cols_keep <- c(names(api_tbl)[1], "slug", "spag")

  }

  ## Define the user agent
  caladaptr_ua <- user_agent(paste0("caladaptr_v", packageVersion("caladaptr")))

  ## Create a tibble to store the results
  res_tbl <- NULL

  ## Determine if this set of API calls uses sf objects
  aoi_sf <- inherits(apicalls_lst$loc_sf, "sf")

  ## Compute the file names which will be used to export individual features to temporary geojson files
  ## There should be one file name for each feature (row)
  if (aoi_sf) {

    # features_hashes <- apicalls_lst$loc_sf %>%
    #   st_geometry() %>%
    #   st_as_text() %>%
    #   digest(algo = "crc32", file = FALSE)

    features_hashes <- unname(sapply(apicalls_lst$loc_sf %>%
                                       st_geometry() %>% st_as_text(),
                                     digest,
                                     algo = "crc32",
                                     file = FALSE))

    gjsn_all_fn <- file.path(ca_cache_dir,
                             paste0("~ca_", features_hashes, ".geojson"))

    if (anyDuplicated(gjsn_all_fn) > 0) warning(red("Duplicate feature hashes(s) encountered"))

  }

  ## If a progress bar is needed, set it up
  use_pb <- !quiet && !debug && (nrow(api_tbl) > 3)
  if (use_pb) pb <- txtProgressBar(min = 1, max = nrow(api_tbl), style = 3)

  ## Loop through calls
  if (debug) message(silver(paste0(" - going to make ", nrow(api_tbl), " api calls")))

  for (i in 1:nrow(api_tbl)) {

    if (use_pb) setTxtProgressBar(pb, i)

    if (aoi_sf) {

      ## Get the temporary geojson file name for this row (which is stored in the loc_qry column)
      gjsn_fn <- gjsn_all_fn[api_tbl[i, "loc_qry", drop = TRUE]]

      if (!file.exists(gjsn_fn)) {
        if (debug) message(silver(paste0(" - saving temp geojson: ", basename(gjsn_fn))))
        ## loc_sf is already projected to 4326
        st_write(apicalls_lst$loc_sf %>%
                   slice(api_tbl[i, "loc_qry", drop = TRUE]),
                 dsn = gjsn_fn,
                 quiet = TRUE)
      }

      ## Create values for parameters (setting them to NULL if not used)
      if (is.na(api_tbl[i, "start", drop = TRUE])) {
        start_dt <- NULL
      } else {
        start_dt <- as.character(api_tbl[i, "start", drop = TRUE])
      }

      if (is.na(api_tbl[i, "end", drop = TRUE])) {
        end_dt <- NULL
      } else {
        end_dt <- as.character(api_tbl[i, "end", drop = TRUE])
      }

      if (is.na(api_tbl[i, "spag", drop = TRUE]) || api_tbl[i, "spag", drop = TRUE] == "none") {
        spatial_ag <- NULL
      } else {
        spatial_ag <- as.character(api_tbl[i, "spag", drop = TRUE])
      }

      body_flds_lst <- list(features = upload_file(gjsn_fn),
                            start = start_dt,
                            end = end_dt,
                            stat = spatial_ag)

      # format = "json" - not using any more

      post_url <- api_tbl[i, "api_url", drop = TRUE]

      if (debug) {
        feat_id <- api_tbl[i, 1, drop = TRUE]
        message(silver(paste0(" - (", i, "/", nrow(api_tbl), ") PUT ", post_url, ". (", feat_id_fldname, "=",
                              feat_id, ", start='", start_dt, "', end='", end_dt, "', stat='", spatial_ag, "')")))
      }

      ## Make the POST request
      qry_resp <- POST(url = post_url,
                       body = body_flds_lst,
                       encode = "multipart",
                       accept_json(),
                       caladaptr_ua)

    } else {   ## SEND A GET REQUEST

      api_url_full <- api_tbl[i, "api_url", drop = TRUE]
      # api_url_short <- as.character(substring(api_url_full, 30, nchar(api_url_full)))

      if (debug) message(silver(paste0(" - (", i, "/", nrow(api_tbl), ") ", api_url_full)))

      qry_resp <- GET(api_url_full, accept_json(), caladaptr_ua)

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

      ## Good response. Convert content to a list
      qry_content <- content(qry_resp, type = "application/json")

      if (length(qry_content) > 0) {

        ## Get the data values (one per date)
        these_vals <- unlist(qry_content$data)

        ## Convert units
        if (!is.na(api_tbl[i, "rs_units", drop = TRUE])) {
          these_vals <- set_units(these_vals,
                                  value = api_tbl[i, "rs_units", drop = TRUE],
                                  mode = "standard")
        }

        ## Append these rows to the tibble
        res_tbl <- rbind(res_tbl,
                         tibble(api_tbl[i, apicall_cols_keep],
                                dt = substr(unlist(qry_content$index), 1, 10),
                                val = these_vals))

      } else {
        ## Nothing returned - date fell outside of range? location outside extent?
        if (debug) message(silver(" - no values returned!"))

      }

    }

  } ## for (i in 1:nrow(api_tbl)) {

  ## Close the progress bar
  if (use_pb) close(pb)

  ## Delete any temporary geojson files created (including this run and previous runs)
  if (aoi_sf) {
    tmp_jsn_fn <- list.files(ca_cache_dir, pattern = "^\\~ca_(.*).geojson$", full.names = TRUE)
    if (length(tmp_jsn_fn) > 0) {
      if (debug) message(silver(paste0(" - deleting ", length(tmp_jsn_fn), " temp geojson files")))
      unlink(tmp_jsn_fn)
    }
  }

  invisible(res_tbl)

}



