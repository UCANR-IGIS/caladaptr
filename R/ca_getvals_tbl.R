#' Get values from an API request object as a tibble
#'
#' @param x A Cal-Adapt API request
#' @param quiet Suppress messages
#' @param debug Print additional output at the console
#' @param stop_on_err Stop if the server returns an error
#'
#' @return A tibble
#'
#' @importFrom crayon bold yellow red silver green magenta
#' @importFrom httr GET POST content content_type_json modify_url user_agent upload_file
#' @importFrom utils txtProgressBar setTxtProgressBar packageVersion
#' @importFrom units set_units
#' @importFrom dplyr select mutate left_join slice
#' @importFrom curl has_internet
#' @importFrom sf st_write
#' @export

ca_getvals_tbl <- function(x, quiet = FALSE, debug = FALSE, stop_on_err = TRUE) {

  if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq")

  ## Check for an internet connection
  if (!has_internet()) stop("No internet connection detected")

  ## Get a tibble with the individual API calls
  apicalls_lst <- ca_apicalls(x)
  api_tbl <- apicalls_lst$api_tbl

  ## Get the name of first column and random string to make the geojson filenames unique to this run
  feat_id_fldname <- apicalls_lst$idfld
  gson_fn_base  <- apicalls_lst$gson_fn_base

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

  # aoi_sf <- !(is.na(apicalls_lst$loc_sf))
  # aoi_sf <- api_tbl[1, "loc_type", drop = TRUE] == "sf"

  ## Compute the file names which will be used to export individual features to temporary geojson files
  ## There should be one file name for each feature (row)
  if (aoi_sf) {
    gjsn_all_fn <- file.path(tempdir(),
                         paste0("~ca_", gson_fn_base,
                                sprintf("_%03d", 1:nrow(apicalls_lst$loc_sf)),
                                ".geojson"))
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

      #gjsn_fn <- file.path(tempdir(), api_tbl[i, "loc_preset", drop = TRUE])

      if (!file.exists(gjsn_fn)) {
        if (debug) message(silver(paste0(" - saving temp geojson: ", basename(gjsn_fn))))
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
                            stat = spatial_ag,
                            format = "json")

      post_url <- api_tbl[i, "api_url", drop = TRUE]

      if (debug) {
        feat_id <- api_tbl[i, 1, drop = TRUE]
        message(silver(paste0(" - (", i, "/", nrow(api_tbl), ") PUT ", post_url, ". (", feat_id_fldname, "=",
                                       feat_id, ", start='", start_dt, "', end='", end_dt, "', stat='", spatial_ag, "', format='json')")))
      }

      ## Make the POST request
      qry_resp <- POST(url = post_url,
                       body = body_flds_lst,
                       encode = "multipart",
                       caladaptr_ua)

      if (qry_resp$status_code == 200) {

        ## Convert response to a list
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


      } else {
        if (debug) message(red(paste0(" - Oh dear. Status code = ", qry_resp$status_code)))
        if (stop_on_err) ca_resp_check(qry_resp, "POST request to query a raster")
      }

      ## END IF AOI_SF
      ###########################################


    } else {

      api_url_full <- api_tbl[i, "api_url", drop = TRUE]
      api_url_short <- as.character(substring(api_url_full, 30, nchar(api_url_full)))

      if (debug) message(silver(paste0(" - (", i, "/", nrow(api_tbl), ") ", api_url_full)))

      ## Make request
      qry_resp <- GET(api_url_full, content_type_json(), caladaptr_ua)

      if (qry_resp$status_code == 200) {

        ## Convert response to a list
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

      } else {

        if (debug) message(red(paste0(" - Oh dear. Status code = ", qry_resp$status_code)))
        if (stop_on_err) ca_resp_check(qry_resp, "Retrieve pixel values")
      }


    }


  } ## for (i in 1:nrow(api_tbl)) {

  ## Close the progress bar
  if (use_pb) close(pb)

  ## Delete any temporary geojson files created (including this run and previous runs)
  if (aoi_sf) {
    tmp_jsn_fn <- list.files(tempdir(), pattern = "^\\~ca_(.*).geojson$", full.names = TRUE)
    if (length(tmp_jsn_fn) > 0) {
      if (debug) message(silver(paste0(" - deleting ", length(tmp_jsn_fn), " temp geojson files")))
      unlink(tmp_jsn_fn)
    }
  }

  invisible(res_tbl)

}



