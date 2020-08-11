#' Get the Cal-Adapt raster series data catalog
#'
#' Get the Cal-Adapt raster series data catalog
#'
#' @param download Download a new copy of the catalog from Cal-Adapt.org
#' @param cache Use a local copy
#' @param quiet Suppress messages
#'
#' @details
#'
#' @importFrom crayon bold yellow
#' @importFrom httr GET content content_type_json
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @export

ca_catalog_rs <- function(download = FALSE, cache = download, quiet = FALSE) {

  ## Returns all available raster series as a data frame
  ## If cache = TRUE, saves the results to disk in the package 'data' folder

  ## TODO
  ## ask S&B when to invalidate the cache RasterSeries

  ## Define some constants
  rs_csv_fn <- "ca_catalog_rs.csv"

  if (download) {
    ## Retrieve from server
    if (!quiet) message(yellow("Getting raster series info from cal-adapt.org..."))

    page_size <- 20
    rseries_url <- paste0(ca_baseurl, "series/")

    rseries_resp <- httr::GET(rseries_url, query=list(pagesize=page_size),
                              httr::content_type_json())
    ca_resp_check(rseries_resp, "retrieve list of raster series")

    ## Parse content into a list
    rseries_data <- httr::content(rseries_resp, type = "application/json")
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
      rseries_resp <- httr::GET(rseries_data[['next']], query = list(pagesize = page_size),
                                httr::content_type_json())
      ca_resp_check(rseries_resp, "retrieve list of raster series")

      ## Parse the response into a list
      rseries_data <- httr::content(rseries_resp, type = "application/json")

      ## Append to the current list
      rseries_res_lst <- c(rseries_res_lst, rseries_data[['results']])

      ## Increment the counter for the progress bar
      if (!quiet) i <- i + 1
    }

    ## Close the progress bar
    if (!quiet) close(pb)

    ## At this point, we're done and have a nice list.
    ## Now convert it to a data frame

    rseries_df <- rseries_res_lst %>% {
      tibble::tibble(
        name = purrr::map_chr(., "name"),
        slug = purrr::map_chr(., "slug"),
        url = purrr::map_chr(., "url"),
        begin = purrr::map_chr(., "begin"),
        end = purrr::map_chr(., "end"),
        num_rast = sapply(purrr::map(rseries_res_lst, "rasters"), length),
        tres = purrr::map_chr(., "tres")
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
    ## Link to the help page for each raster series

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
          if (!quiet) message(yellow("Using raster series catalog from cache"))
          res <- read.csv(file = rs_csv_pathfn)
        }
      }
    }

    ## If that didn't work, use the one that comes with the package
    if (is.null(res)) {
      if (!quiet) message(yellow("Using raster series catalog from package"))
      res <- read.csv(system.file("extdata", rs_csv_fn, package = "caladaptr"))
    }

    ## Return the result
    res

  }

}
