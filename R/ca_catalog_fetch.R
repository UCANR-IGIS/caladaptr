#' Fetch a new copy of the Cal-Adapt raster series catalog
#'
#' Fetch a new copy of the Cal-Adapt raster series catalog
#'
#' @param quiet Suppress messages, logical
#' @param save_to_cache Save the catalog to the cache directory, logical
#'
#' @details This function will download a list of all raster series available through the Cal-Adapt API.
#' If \code{save_to_cache = TRUE}, the catalog will be saved as a csv file in the caladaptR's cache folder
#' and used for subsequent calls to \code{\link{ca_catalog_rs}}.
#'
#' A copy of the raster series catalog is also included with caladaptR. You can run \code{ca_catalog_fetch()}
#' to update the catalog when new raster series are published on Cal-Adapt. The best way to
#' find out when new data are published on Cal-Adapt is to subscribe to the Cal-Adapt newsletter.
#'
#' @return The raster series catalog as a tibble
#'
#' @seealso \code{\link{ca_catalog_rs}}, \code{\link{ca_getcache}}, \code{\link{ca_setcache}}
#'
#' @importFrom purrr map_chr map
#' @importFrom tibble tibble
#' @importFrom httr http_error stop_for_status content GET accept_json
#' @importFrom curl has_internet
#' @import crayon
#' @importFrom utils txtProgressBar setTxtProgressBar

ca_catalog_fetch <- function(quiet = FALSE, save_to_cache = TRUE) {

  ## TODO compare count of slugs to that in in the cache - Smart Cache
  ## where to store and handle the cache
  ## Link to the help page for each raster series

  if (!has_internet()) stop("No internet connection")

  if (!quiet) {
    msg <- getOption("ca_message", paste0)
    message(msg("Fetching raster series catalog from cal-adapt.org..."))
  }

  page_size <- 40

  ## Set up the first URL
  rseries_url <- paste0(ca_baseurl, "series/")
  next_url <- rseries_url

  pb <- NULL
  i <- 0
  rs_catalog_lst <- list()

  while (!is.null(next_url)) {
    i <- i + 1

    ## Get the very first page of data, just to get the total count
    ## We make the first page size = 1 because we just want to get the total count
    rseries_resp <- GET(next_url,
                        query=list(pagesize = page_size),
                        accept_json())

    if (http_error(rseries_resp)) {
      stop_for_status(rseries_resp)

    } else {   ## rseries_resp is good

      ## Parse the contents
      rseries_data <- content(rseries_resp, type = "application/json")

      if (!quiet) {
        if (is.null(pb)) {
          # Setup progress bar
          total <- ceiling(rseries_data$count / page_size)
          pb <- txtProgressBar(min = 1, max = total, style = 3)
        }
        setTxtProgressBar(pb, i)
      }

      ## Next we loop thru the raster series in this page. We get first raster store and ,
      ## grab the units and extent Then we save those as new elements of the rseries_data[['results']]

      for (k in 1:length(rseries_data[['results']])) {

        ## rstore_url <- paste0(rseries_data[['results']][[k]]$rasters[[1]], "?format=json")
        rstore_url <- rseries_data[['results']][[k]]$rasters[[1]]
        # example: https://api.cal-adapt.org/api/rstores/ET_month_ACCESS1-0_historical_1950-01-01/?format=json

        rstore_resp <- GET(rstore_url, accept_json())

        if (http_error(rstore_resp)) stop_for_status(rstore_resp)

        ## Parse content into a list
        rstore_data <- content(rstore_resp, type = "application/json")

        ## Add the first raster's units to the raster series list
        rseries_data[['results']][[k]]$units <- ifelse(is.null(rstore_data$units),
                                                       NA, rstore_data$units)

        ## Also get the extent
        coords_mat <- matrix(data=unlist(rstore_data$geom$coordinates[[1]]), ncol=2, byrow = TRUE)
        xrng <- range(coords_mat[,1])
        yrng <- range(coords_mat[,2])
        rseries_data[['results']][[k]]$xmin <- xrng[1]
        rseries_data[['results']][[k]]$xmax <- xrng[2]
        rseries_data[['results']][[k]]$ymin <- yrng[1]
        rseries_data[['results']][[k]]$ymax <- yrng[2]

        ## Other list elements available:
        ## "id", "tileurl", "url", "width", "height", "geom", "event", "srs",
        ## "minval", "maxval", "nodata", "xpixsize", "ypixsize", "image", "name", "slug", "units"
      }

      ## Parse the results into a list and save it
      rs_catalog_lst <- c(rs_catalog_lst, rseries_data[['results']])

      next_url <- rseries_data[['next']]

    }

  }
  if (!quiet) close(pb)

  ## Convert the list to a tibble
  rseries_tbl <- rs_catalog_lst %>% {
    tibble(
      id = 1:length(.),
      name = map_chr(., "name"),
      slug = map_chr(., "slug"),
      url = map_chr(., "url"),
      begin = map_chr(., "begin"),
      end = map_chr(., "end"),
      num_rast = sapply(map(rs_catalog_lst, "rasters"), length),
      tres = map_chr(., "tres"),
      units = map_chr(., "units"),
      xmin = map_dbl(., "xmin"),
      xmax = map_dbl(., "xmax"),
      ymin = map_dbl(., "ymin"),
      ymax = map_dbl(., "ymax")
    )
  }

  if (save_to_cache) {
    ## Write to disk
    if (!quiet) message(msg(" - saving raster series catalog to cache"))
    rs_csv_fn <- file.path(ca_getcache(), "ca_catalog_rs.csv")
    write.csv(rseries_tbl, file = rs_csv_fn, row.names = FALSE)
  }

  if (!quiet) {
    success <- getOption("ca_success", paste0)
    message(success("Done"))
  }

  ## Return the tibble
  invisible(rseries_tbl)

}
