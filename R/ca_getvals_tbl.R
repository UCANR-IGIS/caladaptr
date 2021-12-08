#' Get values from an API request object as a tibble
#'
#' @param x A Cal-Adapt API request
#' @param quiet Suppress messages
#' @param debug Print additional output at the console
#' @param stop_on_err Stop if the server returns an error
#' @param shiny_progress A Shiny progress bar object, see Details.
#' @param omit_col Columns to exclude from the tibble
#' @param timeout Timeout limit in seconds
#'
#' @details \code{ca_getvals_tbl} fetches data via the Cal-Adapt API, returning a tibble. Everything is done in memory. To download Cal-Adapt into
#' a local SQLite database, see \code{\link{ca_getvals_db}}. To download Cal-Adapt data as raster files, see \code{\link{ca_getrst_stars}}.
#'
#' A default set of columns will be returned based on how the dataset is specified (i.e., by slug, cvar+scen+gcm+per, livneh, etc). Some columns
#' can be omitted by passing column names to \code{col_omit}. Three columns that can never be omitted are \code{feat_id} (location id value),
#' \code{dt} (date), and \code{val} (the actual climate values).
#'
#' \code{timeout} set the longest amount of time before curl reports an error. The default is 10 seconds. Increase this if
#' you experience timeout errors (which have been know to occur on ShinyApps.io perhaps due to server congestion).
#'
#' @return A tibble
#'
#' @seealso \code{\link{ca_getvals_db}}, \code{\link{ca_getrst_stars}}
#'
#' @importFrom crayon red
#' @importFrom httr GET POST content modify_url user_agent upload_file accept_json http_error stop_for_status warn_for_status http_status config
#' @importFrom utils txtProgressBar setTxtProgressBar packageVersion
#' @importFrom units set_units
#' @importFrom dplyr select mutate left_join slice
#' @importFrom curl has_internet
#' @importFrom sf st_write st_geometry st_as_text
#' @importFrom digest digest
#' @importFrom shiny Progress
#' @export

ca_getvals_tbl <- function(x, quiet = FALSE, debug = FALSE, stop_on_err = TRUE, shiny_progress = NULL, omit_col = NULL,
                           timeout = NULL) {

  if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq")

  ## Check for an internet connection
  if (!has_internet()) stop("No internet connection detected")

  ## Get the functions to format messages
  accent2 <- getOption("ca_accent2", I)
  msg_fmt <- getOption("ca_message", I)

  ## Get a tibble with the individual API calls
  apicalls_lst <- ca_apicalls(x, check_for = "getvals")
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

  ## Define which columns to save in the output table
  if (!identical(x$slug, NA)) {
    apicall_cols_default <- c(names(api_tbl)[1], "slug", "spag")

  } else if (identical(x$livneh, TRUE)) {
    apicall_cols_default <- c(names(api_tbl)[1], "cvar", "period", "slug", "spag")

  } else {
    apicall_cols_default <- c(names(api_tbl)[1], "cvar", "period", "gcm", "scenario", "spag")

  }

  if (!is.null(omit_col)) {
    if ("val" %in% omit_col) stop("Sorry, you can not omit the values column from the results")
    if ("dt" %in% omit_col) stop("Sorry, you can not omit the date column from the results")
    if (names(api_tbl)[1] %in% omit_col) {
      stop("Sorry, you can not omit the location column from the results")
    }
  }

  apicall_cols_keep <- setdiff(apicall_cols_default, omit_col)
  ## cat("\napicall_cols_keep = ", paste(apicall_cols_keep, collapse = ", "), "\n")

  ## Define the user agent
  caladaptr_ua <- user_agent(paste0("caladaptr_v", packageVersion("caladaptr")))

  ## Set the timeout option
  if (is.null(timeout)) {
    tout_config <- NULL
  } else {
    ## httr::timeout(timeout) ## this seemed to have no effect on ShinyApps.io
    if (!is.numeric(timeout)) stop("timeout must be numeric (number of seconds)")
    tout_config <- httr::config(connecttimeout = timeout)
  }

  ## Create a tibble to store the results
  res_tbl <- NULL

  ## Determine if this set of API calls uses sf objects
  aoi_sf <- inherits(apicalls_lst$loc_sf, "sf")

  ## Compute the file names which will be used to export individual features to temporary geojson files
  ## There should be one file name for each feature (row)
  if (aoi_sf) {

    features_hashes <- unname(sapply(apicalls_lst$loc_sf %>%
                                       st_geometry() %>% st_as_text(),
                                     digest,
                                     algo = "crc32",
                                     file = FALSE))

    gjsn_all_fn <- file.path(ca_cache_dir,
                             paste0("~ca_", features_hashes, ".geojson"))

    if (anyDuplicated(gjsn_all_fn) > 0) warning(red("Duplicate feature hashes(s) encountered"))

  }

  ## Do we need a progress bar?
  use_pb <- !is.null(shiny_progress) || (!quiet && !debug && (nrow(api_tbl) > 3))

  ## If a text progress bar is needed for the console, set it up
  if (use_pb && is.null(shiny_progress)) {
    pb <- txtProgressBar(min = 1, max = nrow(api_tbl), style = 3)
  }

  ## Loop through calls
  if (debug) message(msg_fmt(paste0(" - going to make ", nrow(api_tbl), " api calls")))

  for (i in 1:nrow(api_tbl)) {

    if (use_pb) {
      if (is.null(shiny_progress)) {
        setTxtProgressBar(pb, i)
      } else {
        shiny_progress$inc(1/nrow(api_tbl))
      }
    }

    if (aoi_sf) {

      ## Get the temporary geojson file name for this row (which is stored in the loc_qry column)
      gjsn_fn <- gjsn_all_fn[api_tbl[i, "loc_qry", drop = TRUE]]

      if (!file.exists(gjsn_fn)) {
        if (debug) message(msg_fmt(paste0(" - saving temp geojson: ", basename(gjsn_fn))))
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

      post_url <- api_tbl[i, "api_url", drop = TRUE]

      if (debug) {
        feat_id <- api_tbl[i, 1, drop = TRUE]
        message(msg_fmt(paste0(" - (", i, "/", nrow(api_tbl), ") PUT ", post_url, ". (", feat_id_fldname, "=",
                              feat_id, ", start='", start_dt, "', end='", end_dt, "', stat='", spatial_ag, "')")))
      }

      ## Make the POST request
      qry_resp <- POST(url = post_url,
                       body = body_flds_lst,
                       encode = "multipart",
                       accept_json(),
                       caladaptr_ua,
                       tout_config)

    } else {   ## SEND A GET REQUEST
      api_url_full <- api_tbl[i, "api_url", drop = TRUE]
      if (debug) message(msg_fmt(paste0(" - (", i, "/", nrow(api_tbl), ") ", api_url_full)))
      qry_resp <- GET(api_url_full, accept_json(), caladaptr_ua, tout_config)
    }

    ## At this point we have a response object

    ## See if the server sent an error
    if (http_error(qry_resp)) {

      ## Should we stop because there was a server error?
      if (stop_on_err) {
        stop_for_status(qry_resp)

      } else {

        ## If not, generate message or warning
        if (quiet && !debug) {
          warn_for_status(qry_resp)

        } else {
          message(red(paste0(" - Oh dear. ", http_status(qry_resp)$message)))
        }

      }


    } else {

      ## Good response. Convert content to a list
      qry_content <- content(qry_resp, type = "application/json")

      if (length(qry_content) > 0) {

        ## Get the data values (one per date)
        these_vals <- unlist(qry_content$data)

        ## Set units
        rs_units_chr <- api_tbl[i, "rs_units", drop = TRUE]
        if (!is.na(rs_units_chr)) {
          if (rs_units_chr == "C") {rs_units_chr <- "celsius"}
          these_vals <- set_units(these_vals,
                                  value = rs_units_chr,
                                  mode = "standard")
        }

        ## Append these rows to the tibble
        res_tbl <- rbind(res_tbl,
                         tibble(api_tbl[i, apicall_cols_keep],
                                dt = substr(unlist(qry_content$index), 1, 10),
                                val = these_vals))

      } else {
        ## Nothing returned - date fell outside of range? location outside extent?
        if (debug) message(msg_fmt(" - no values returned!"))

      }

    }

  } ## for (i in 1:nrow(api_tbl)) {

  ## Close the progress bar
  if (use_pb && is.null(shiny_progress)) close(pb)

  ## Delete any temporary geojson files created (including this run and previous runs)
  if (aoi_sf) {
    tmp_jsn_fn <- list.files(ca_cache_dir, pattern = "^\\~ca_(.*).geojson$", full.names = TRUE)
    if (length(tmp_jsn_fn) > 0) {
      if (debug) message(msg_fmt(paste0(" - deleting ", length(tmp_jsn_fn), " temp geojson files")))
      unlink(tmp_jsn_fn)
    }
  }

  invisible(res_tbl)

}



