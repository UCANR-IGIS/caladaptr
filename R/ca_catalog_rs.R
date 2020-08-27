#' Get the Cal-Adapt raster series data catalog
#'
#' Get the Cal-Adapt raster series data catalog
#'
#' @param download Download a new copy of the catalog from Cal-Adapt.org
#' @param cache Use a local copy
#' @param quiet Suppress messages
#'
#' @details
#' This retrieves a 'catalog' of the raster series available through the Cal-Adapt API.
#' Normally, you can use the copy which comes with caladaptr, but to force download a new copy,
#' use \code{download = TRUE}. This is only needed when new
#' datasets are added to the API (subscribe to the Cal-Adapt news letter for notifications).
#'
#' @return A data frame (tibble) with columns of information about the raster series available through
#' the Cal-Adapt API.
#'
#' @seealso \code{\link{ca_getcache}}
#'
#' @importFrom crayon bold yellow
#' @importFrom purrr map map_chr map_dbl
#' @importFrom tibble tibble
#' @importFrom httr GET content content_type_json
#' @importFrom utils txtProgressBar setTxtProgressBar head read.csv write.csv
#'
#' @export

ca_catalog_rs <- function(download = FALSE, cache = download, quiet = FALSE) {

  ## If cache = TRUE, saves the results to disk in the package 'data' folder

  ## TODO
  ## ask S&B when to invalidate the cache Raster Series

  ## Define some constants
  rs_csv_fn <- "ca_catalog_rs.csv"

  if (download) {
    ## Retrieve from server
    if (!quiet) message(yellow("Getting raster series info from cal-adapt.org..."))

    page_size <- 20
    rseries_url <- paste0(ca_baseurl, "series/")

    rseries_resp <- GET(rseries_url, query=list(pagesize=page_size),
                              content_type_json())
    ca_resp_check(rseries_resp, "retrieve list of raster series")

    ## Parse content into a list
    rseries_data <- content(rseries_resp, type = "application/json")

    ## Next we loop thru each of these raster series, grab the first raster, and record the units
    for (k in 1:length(rseries_data[['results']])) {
      rstore_url <- paste0(rseries_data[['results']][[k]]$rasters[[1]], "?format=json")
      # example: https://api.cal-adapt.org/api/rstores/ET_month_ACCESS1-0_historical_1950-01-01/?format=json

      rstore_resp <- GET(rstore_url, content_type_json())
      ca_resp_check(rstore_resp, "retrieve raster store properties")

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

    ## Start rseries_res_lst with this page
    rseries_res_lst <- rseries_data[['results']]

    if (!quiet) {
      # Setup progress bar
      total <- ceiling(rseries_data$count / page_size)
      pb <- txtProgressBar(min = 1, max = total, style = 3)
      i <- 2
    }

    while (!is.null(rseries_data[['next']])) {
      # Update progress bar
      if (!quiet) {setTxtProgressBar(pb, i)}

      ## Get the next page
      rseries_resp <- GET(rseries_data[['next']], query = list(pagesize = page_size),
                                content_type_json())
      ca_resp_check(rseries_resp, "retrieve list of raster series")

      ## Parse the response into a list
      rseries_data <- content(rseries_resp, type = "application/json")

      ## Next we loop thru each of these raster series, grab the first raster, record the units & extent
      for (k in 1:length(rseries_data[['results']])) {
        rstore_url <- paste0(rseries_data[['results']][[k]]$rasters[[1]], "?format=json")
        rstore_resp <- GET(rstore_url, content_type_json())
        ca_resp_check(rstore_resp, "retrieve raster store properties")
        rstore_data <- content(rstore_resp, type = "application/json")

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

      }

      #message(paste(sapply(rseries_data[['results']], length), collapse = ", "))

      ## Append to the current list
      rseries_res_lst <- c(rseries_res_lst, rseries_data[['results']])

      ## Increment the counter for the progress bar
      if (!quiet) i <- i + 1
    }

    ## Close the progress bar
    if (!quiet) close(pb)

    ## Now convert the list to a data frame
    rseries_df <- rseries_res_lst %>% {
      tibble(
        name = map_chr(., "name"),
        slug = map_chr(., "slug"),
        url = map_chr(., "url"),
        begin = map_chr(., "begin"),
        end = map_chr(., "end"),
        num_rast = sapply(map(rseries_res_lst, "rasters"), length),
        units = map_chr(., "units"),
        xmin = map_dbl(., "xmin"),
        xmax = map_dbl(., "xmax"),
        ymin = map_dbl(., "ymin"),
        ymax = map_dbl(., "ymax"),
        tres = map_chr(., "tres")
      )
    }

    if (cache) {
      cache_dir <- ca_getcache()
      if (!is.na(cache_dir)) {
        if (!quiet) message(yellow("Saving raster series catalog to cache"))
        write.csv(rseries_df, file = file.path(cache_dir, rs_csv_fn), row.names = FALSE)
      }
    }

    ## TODO
    ## Link to the help page (dataset id number) for each raster series

    ## Return the data frame
    invisible(rseries_df)

  } else {

    ## Download = FALSE. In that case we look 1) in the cache dir (if cache=T), and if
    ## we don't find anything there we get the one bundled with the package.

    res <- NULL

    if (cache) {
      cache_dir <- ca_getcache(default = TRUE, quiet = TRUE)
      if (is.na(cache_dir)) {
        warning("Can't access cache directory")
      } else {
        rs_csv_pathfn <- file.path(cache_dir, rs_csv_fn)
        if (file.exists(rs_csv_pathfn)) {
          if (!quiet) message(yellow(" - using raster series catalog from cache"))
          res <- read.csv(file = rs_csv_pathfn)
        }
      }
    }

    ## If that didn't work, use the one that comes with the package
    if (is.null(res)) {
      if (!quiet) message(yellow(" - using the raster series catalog bundled with caladaptr"))
      res <- read.csv(system.file("extdata", rs_csv_fn, package = "caladaptr"))
    }

    ## Return the result
    res

  }

}
