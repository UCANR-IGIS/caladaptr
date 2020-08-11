#' Retrieves Cal-Adapt data catalog
#'
#' Retrieves Cal-Adapt data catalog
#'
#' @param aoipreset AOI preset(s)
#' @param download Download new catalog from Cal-Adapt
#' @param cache Use local cache (see details)
#' @param save_aoigeom Save the geom for AOI preset
#' @param max_pages Maximum number of pages to retrieve
#' @param quiet Suppress messages
#'
#' @details
#' \code{aoipreset} can be one of the values in \code{ca_aoipreset}.
#'
#' If \code{cache=TRUE} and \code{download=FALSE}, it will look in the cache directory
#' before using the catalog bundled with \code{caladaptr}.  If \code{download=TRUE} and
#' \code{cache=TRUE}, the new results will be saved.
#'
#' @return A named list of data frames. Each list element corresponds to one of the
#' AOI Preset types (e.g., counties). The data frame includes the columns which uniquely
#' identify each feature (e.g., name, fips code). #'
#'
#' @export
#'
#' @importFrom crayon bold yellow
#' @importFrom sf st_read st_write
#' @importFrom jsonlite read_json
#' @importFrom utils txtProgressBar setTxtProgressBar

ca_catalog_aoipreset <- function(aoipreset = ca_aoipreset,
                          download = TRUE,
                          cache = TRUE,
                          save_aoigeom = TRUE,
                          max_pages = NULL,
                          quiet = FALSE) {

  if (!quiet && is.null(aoipreset)) {
    warning("Nothing to fetch. Pass a value for aoipreset.")
  }

  if (FALSE %in% (aoipreset %in% ca_aoipreset)) {
    badvals <- paste(aoipreset[!aoipreset %in% ca_aoipreset], collapse = ",")
    stop(paste0("Unknown value(s) in aoipreset: ", badvals,
                "\nFor valid values, see ca_aoipreset."))
  }

  ## Define some constants
  res <- list()
  page_size <- 10
  tmp_fn <- tempfile(fileext = ".geojson")

  for (preset in aoipreset) {

    if (download) {

      if (!quiet) message(yellow(paste0("Retrieving ", preset)))
      next_url <- paste0(ca_baseurl, preset, "/?format=geojson&page=1")

      ## Query the AOI preset resource as a GeoJSON object
      aoi_geo_resp <- httr::GET(next_url, query=list(pagesize=page_size))
      ca_resp_check(aoi_geo_resp, paste0("retrieve AOI preset for ", preset))

      ## Extract content as text
      aoi_geo_txt <- httr::content(aoi_geo_resp, as = "text")

      ## Save text to temporary file. Although not strictly necessary, this may
      ## be faster and kinder on memory.
      cat(aoi_geo_txt, file = tmp_fn, sep = "\n")

      ## Next, we need to parse it as a JSON object, so we can get the ID values
      ## as well as the count for the progress bar
      aoi_lst <- jsonlite::read_json(tmp_fn, simplifyVector = FALSE)

      ## Import as a SF object
      aoi_sf <- st_read(tmp_fn, quiet = TRUE)

      ## Add the id column
      aoi_sf$id <- sapply(aoi_lst$features, function(x) x$id)

      ## That completes the first page. Next loop through the remaining pages
      num_recs <- aoi_lst$count

      if (!quiet) {
        # Setup a progress bar
        total <- ceiling(num_recs / page_size)
        if (!is.null(max_pages)) total <- min(max_pages, total)
        pb <- txtProgressBar(min = 1, max = total, style = 3)
        i <- 2
      }

      while (!is.null(aoi_lst[['next']])) {
        if (!quiet) {
          # update progress bar
          setTxtProgressBar(pb, i)
        }

        ## Query the AOI preset resource as a GeoJSON object
        aoi_geo_resp <- httr::GET(aoi_lst[['next']], query=list(pagesize=page_size))
        ca_resp_check(aoi_geo_resp, paste0("retrieve AOI preset for ", preset))

        ## Extract content as text
        aoi_geo_txt <- httr::content(aoi_geo_resp, as = "text")

        ## Save it to the temp file
        cat(aoi_geo_txt, file = tmp_fn, sep = "\n")

        ## Read it in as a plain JSON object
        aoi_lst <- jsonlite::read_json(tmp_fn, simplifyVector = FALSE)

        ## Read with st_read
        aoimore_sf <- st_read(tmp_fn, quiet = TRUE)

        ## Add the id column
        aoimore_sf$id <- sapply(aoi_lst$features, function(x) x$id)

        ## Append it
        aoi_sf <- rbind(aoi_sf, aoimore_sf)

        ## Increment i for the progress bar
        if (!quiet) i <- i + 1

        ## Check if we should stop here
        if (!is.null(max_pages)) {
          if (i > max_pages) break
        }

      } ## while TRUE paging through the API calls

      if (!quiet) close(pb)

      ## Create the data frame with just the column(s) containing the ID values
      idvals_flds <- ca_aoipreset_idfld[[preset]]
      idvals_tbl <- aoi_sf %>% st_drop_geometry() %>% select(idvals_flds)

      ## Add this data frame to the result list
      res[[preset]] <- idvals_tbl

      if (cache) {
        ## Because most users will only work with one kind of AOI, its
        ## most efficient to save id values in separate tables.

        ## Grab the cache directory
        cache_dir <- ca_getcache(default = TRUE, quiet = TRUE)

        ## Construct a filename for the idvals
        idvals_fn <- file.path(cache_dir, paste0(preset, "_idvals.rda"))

        ## Save data frame
        save(idvals_tbl, file = idvals_fn)

        ## If required, save the geometry as a geopackage
        if (save_aoigeom) {
          gpkg_fn <- file.path(cache_dir, paste0(preset, ".gpkg"))
          st_write(aoi_sf, gpkg_fn, delete_dsn = file.exists(gpkg_fn))
        }

      }

      ## Clean up
      if (file.exists(tmp_fn)) file.remove(tmp_fn)

      ## end IF DOWNLOAD

    } else {
      stop("Retrieving bundled and/or cached catalog not yet supported")
    }

  } # for each preset in aoipreset

  ## Return the result
  res

}


