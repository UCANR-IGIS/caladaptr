#' Get values from an API request object
#'
#' @param x A Cal-Adapt API request
#' @param quiet Suppress messages
#' @param debug Print additional output at the console
#'
#' @importFrom crayon bold yellow red silver
#' @importFrom httr GET content content_type_json modify_url
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom sf st_as_text st_point
#' @importFrom units set_units
#' @export

ca_getvals <- function(x, quiet = FALSE, debug = FALSE, use_events = FALSE) {

  if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq")

  ## Error checks on the API request (to be moved to its own function)
  if (identical(x$loc, NA)) stop("A location must be specified")
  if (identical(x$gcm, NA)) stop("A gcm must be specified")
  if (identical(x$scenario, NA)) stop("An emissions scenario must be specified")
  if (identical(x$per, NA)) stop("A period must be specified")
  if (identical(x$cvar, NA)) stop("Climate variable must be provided")

  num_loc <- switch(x$loc$type,
                    sf = nrow(x$loc$val),
                    aoipreset = length(x$loc$val$idval),
                    zip = length(x$loc$val),
                    pt = nrow(x$loc$val),
                    stop("unknown value for loc$type"))

  page_size <- 10

  ## Get the raster series data catalog
  rs_df <- ca_catalog_rs(quiet = TRUE)
  slugs_lc <- tolower(rs_df$slug)

  ## Note we need to support paging
  ## for (myloc_page_idx in 1:ceiling(num_loc / page_size)) {

  res <- list()

  for (myloc_idx in 1:num_loc) {

    call_api <- TRUE

    g_qrylst <- list()
    ref_qrylst <- list()

    ## Construct the qry_geom
    if (x$loc$type == "pt") {

      ## Create the g parameter list
      g_qrylst <- list(g = st_as_text(st_point(as.numeric(x$loc$val[myloc_idx, c(2,3)]))))

      ## Need to create a feature ID as a character object because it will be used as a list element name
      feat_id <- as.character(x$loc$val[myloc_idx, 1])

      if (is.null(attr(res, "idfld"))) {
        attr(res, "idfld") <- list(name = names(x$loc$val)[1], class = class(x$loc$val[, 1]))
      }

      if (!quiet) message(yellow$bold("POINT #", myloc_idx, sep = ""))

    } else if (x$loc$type == "aoipreset") {

      ## Get the type, fld, and identifying value for the feature
      preset <- x$loc$val$type
      idfld <- x$loc$val$idfld
      idval <- x$loc$val$idval[myloc_idx]

      ## Find the row in the lookup table
      aoipreset_idx <- which (aoipreset_idval[[preset]][[idfld]]  == idval)

      ## Get the value of the 'id' column
      api_featid <- aoipreset_idval[[preset]]$id[aoipreset_idx]

      ## Construct the ref query parameter
      ref_qrylst <- list(ref = paste0("/api/", preset, "/", api_featid, "/"))

      #feat_id <- gsub(" ", "_", paste(idfld, idval, sep = "_"))
      feat_id <- as.character(idval)

      if (is.null(attr(res, "idfld"))) attr(res, "idfld") <- list(name = idfld, class = class(idval))

      if (!quiet) message(yellow$bold("\n", toupper(preset), ": ", idfld, " = ", idval, sep = ""))

    } else if (x$loc$type == "zip") {
      if (!quiet) message(red("Sorry, querying zip codes is not yet supported"))
      call_api <- FALSE

    } else if (x$loc$type == "sf") {
      if (!quiet) message(red("Sorry, querying sf locations is not yet supported"))
      call_api <- FALSE

    }

    if (call_api) res[[feat_id]] <- list()

    for (mycvar in x$cvar) {
      if (call_api) res[[feat_id]][[mycvar]] <- list()

      for (myper in x$period) {
        if (call_api) res[[feat_id]][[mycvar]][[myper]] <- list()

        for (mygcm in x$gcm) {
          if (call_api) res[[feat_id]][[mycvar]][[myper]][[mygcm]] <- list()

          for (myscenario in x$scenario) {
            if (call_api) res[[feat_id]][[mycvar]][[myper]][[mygcm]][[myscenario]] <- list()

            ## Prep the options
            if (identical(x$options, NA)) {
              spatial_ags <- "none"
            } else {
              spatial_ags <- x$options$spatial_ag
            }

            for (myspag in spatial_ags) {

              stat_qrylst <- list()

              ## Need some compatibility checking right here
              if (x$loc$type == "aoipreset") {    ## OR SF-POLYGON
                if (myspag == "none" || is.na(myspag)) {
                  if (!quiet) message(red("Spatial aggregation function required. See `ca_options`. "))
                  call_api <- FALSE
                } else {
                  stat_qrylst <- list(stat = myspag)
                }
              }  else {
                ## Will not do any checking on spatial ag for point locations, just ignore them
              }

              ## Initialize a new element, setting it to NULL (will become a data frame)
              res[[feat_id]][[mycvar]][[myper]][[mygcm]][[myscenario]][[myspag]] <- NULL

              ## If everything is still ok, proceed
              if (call_api) {

                ## Construct a slug
                myslug <- paste(mycvar, myper, mygcm, myscenario, sep="_")

                ## See if this slug is in the data catalog
                slug_idx <-  which(tolower(myslug) == slugs_lc)

                if (length(slug_idx) > 0) {

                  if (!quiet) message(yellow(paste0(rs_df[slug_idx, "name"])))

                  if (use_events) {

                    startend_qrylst <- list()

                    if (!identical(x$dates, NA)) {
                      if (debug) message(silver(" - cross-check dates against per ??"))
                      startend_qrylst <- list(start = x$dates$start, end = x$dates$end)
                    }
                      ## Construct the query parameters (empty lists will 'drop out')
                    qry_params <- c(g_qrylst, ref_qrylst, stat_qrylst, startend_qrylst, list(format='json'))

                    #&start=2040-01-01&end=2060-01-01
                    #https://api.cal-adapt.org/api/series/tasmax_year_HadGEM2-ES_rcp45/events/?g=POINT%20%28-121.4687%2038.5938%29&start=2040-01-01&end=2060-01-01&format=json

                    ## Construct the URL
                    qry_url <- paste0(ca_baseurl, "series", "/", myslug, "/events/")

                    ## writeClipboard(httr::modify_url(qry_url, query=qry_params))
                    if (debug) message(silver(modify_url(qry_url, query=qry_params)))

                    ## Make request
                    qry_resp <- httr::GET(qry_url, query=qry_params, content_type_json())
                    ca_resp_check(qry_resp, "retrieve pixel values at a location")

                    ## Convert response to a list
                    qry_content <- content(qry_resp, type = "application/json")

                    ## Getting the units
                    ## To get the units, I would need to grab the first raster in the series
                    ## so query this
                    ## https://api.cal-adapt.org/api/series/tasmax_year_HadGEM2-ES_rcp45/
                    ## from the rasters list, get the first one, the URL for the raster store will look like:
                    ## https://api.cal-adapt.org/api/rstores/tasmax_year_HadGEM2-ES_rcp45_2006/
                    ## in this raster store you'll see the units

                    res[[feat_id]][[mycvar]][[myper]][[mygcm]][[myscenario]][[myspag]] <- data.frame(
                      dt = substr(unlist(qry_content$index), 1, 10),
                      val = unlist(qry_content$data))

                  } else {

                    ## Construct the query parameters (empty lists will 'drop out')
                    qry_params <- c(g_qrylst, ref_qrylst, stat_qrylst, list(pagesize=10, format='json'))

                    if (identical(x$dates, NA)) {
                      rasters_piece <- "rasters"

                    } else {
                      start_use <- x$dates$start
                      end_use <- x$dates$end

                      ## If period is month or year, use the first day of the month/year
                      if (myper == "year") {
                        if (substr(start_use, 6, 10) != "01-01") {
                          start_use <- paste0(substr(start_use, 1, 5), "01-01")
                          if (!quiet) message(red("Adjusting start date to", start_use))
                        }
                        if (substr(end_use, 6, 10) != "01-01") {
                          end_use <- paste0(substr(end_use, 1, 5), "01-01")
                          if (!quiet) message(red("Adjusting end date to", end_use))
                        }
                      }

                      if (myper == "month") {
                        if (substr(start_use, 9, 10) != "01") {
                          start_use <- paste0(substr(start_use, 1, 8), "01")
                          if (!quiet) message(red("Adjusting start date to", start_use))
                        }
                        if (substr(end_use, 9, 10) != "01") {
                          end_use <- paste0(substr(end_use, 1, 8), "01")
                          if (!quiet) message(red("Adjusting end date to", end_use))
                        }
                      }

                      rasters_piece <- paste0(start_use, "/", end_use)

                    }

                    ## Construct the URL
                    qry_url <- paste0(ca_baseurl, "series", "/", myslug, "/", rasters_piece, "/")

                    ## Make request
                    ## writeClipboard(httr::modify_url(qry_url, query=qry_params))
                    # browser()
                    if (debug) message(silver(modify_url(qry_url, query=qry_params)))

                    qry_resp <- httr::GET(qry_url, query=qry_params, content_type_json())
                    ca_resp_check(qry_resp, "retrieve pixel values at a location")

                    ## Convert response to a list
                    qry_content <- httr::content(qry_resp, type = "application/json")

                    ## View how many there are
                    count_vals <- qry_content$count

                    if (count_vals == 0) {
                      if (!quiet) message(red(" - no values returned :-("))
                      ## if (!quiet) message(red("   ", qry_url, sep=""))
                      if (!quiet) message(red("   ", modify_url(qry_url, query=qry_params), sep=""))
                    } else {

                      ## Grab the units from the first one
                      rstore_units <- qry_content$results[[1]]$units
                      units_str <- switch(rstore_units,
                                          K = 'K',
                                          "mm/day" = "mm/d",
                                          NULL)
                      if (is.null(units_str)) message(red(paste0(" - weird units found: ", rstore_units, ". Units will not be saved in the values returned.")))

                      ## for temp it looks like 'K'
                      ## for pr it looks like "mm/day"

                      ## View Pixel Values at this Point
                      vals_this_page <- set_units(sapply(qry_content$results, function(x) x$image),
                                                  units_str, mode = "standard")

                      ## View dates
                      dates_this_page <- sapply(qry_content$results, function(x) x$event)

                      ## Store those somewhere, remember the units!
                      res[[feat_id]][[mycvar]][[myper]][[mygcm]][[myscenario]][[myspag]] <- rbind(
                        res[[feat_id]][[mycvar]][[myper]][[mygcm]][[myscenario]][[myspag]],
                        data.frame(dt=dates_this_page, val=vals_this_page)
                      )

                      ## For debugging purposes, show the 'next' URL
                      ## cat(qry_content[['next']], "\n")

                      ## If a progress bar is needed, set it up
                      use_pb <- !quiet && !debug && (count_vals > page_size) && (count_vals > 10)
                      if (use_pb) {
                        # Setup progress bar
                        total_pages <- ceiling(count_vals / page_size)
                        pb <- txtProgressBar(min = 1, max = total_pages, style = 3)
                        i <- 2
                      }

                      while (!is.null(qry_content[['next']])) {
                        # Update progress bar
                        if (use_pb) {setTxtProgressBar(pb, i)}

                        ## Make request
                        qry_resp <- httr::GET(qry_content[['next']], query=qry_params, content_type_json())
                        ca_resp_check(qry_resp, "retrieve pixel values at a location")

                        ## Convert response to a list
                        qry_content <- httr::content(qry_resp, type = "application/json")

                        ## View Pixel Values at this Point
                        vals_this_page <- set_units(sapply(qry_content$results, function(x) x$image),
                                                    units_str, mode = "standard")

                        ## View dates
                        dates_this_page <- sapply(qry_content$results, function(x) x$event)

                        ## Store those somewhere
                        ## Store those somewhere, remember the units!
                        res[[feat_id]][[mycvar]][[myper]][[mygcm]][[myscenario]][[myspag]] <- rbind(
                          res[[feat_id]][[mycvar]][[myper]][[mygcm]][[myscenario]][[myspag]],
                          data.frame(dt=dates_this_page, val=vals_this_page)
                        )

                        if (use_pb) i <- i + 1
                      }
                      ## Close the progress bar
                      if (use_pb) close(pb)

                    }   ## if count > 0


                  }

                } else {   ## if (length(slug_idx) > 0) {
                  if (!quiet) message(red("Sorry,", myslug, " does not seem to be available on Cal-Adapt"))
                }

              }

            }


          }
        }

      }
    }

  }

  ## Return the result
  class(res) <- c("ca_qryvals_lst", "list")
  res

}

## Notes

# loc is a list object with two elements
# type = c("sf", "aoipreset", "zip", "pt")

# when type = "sf", val = a sf data frame

# when type = "aoipreset", val should be a list with three elements:
#  val = list(type = "counties",
#              idfld = "fips",
#              idvals = c("23425", "23425", "03824"))

# when type = "zip", val = a character vector of zip codes (US)

# when type = "pt", val = a two-column matrix or data frame (col1= lon, col2=lat)

