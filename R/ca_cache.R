#' Manage cache directory
#'
#' View and set the directory for the data catalog
#'
#' @param quiet Show messages, logical
#'
#' @details \code{caladaptr} has the ability to store copies of objects downloaded
#' from Cal-Adapt. An example of this would be the raster series data catalog and the
#' geometries of AOI Presets.
#'
#' NOTE: In general \code{caladaptr} does \emph{not} cache climate data fetched from Cal-Adapt. Every time
#' you call a function that fetches data (e.g., \code{\link{ca_getvals_tbl}}), data is retrieved fresh.
#' The exception to this is \code{\link{ca_getvals_db}}, which has arguments you can pass to cache retrieved
#' values into a local SQLite database to explicitly avoid downloading data twice.
#'
#' The default location for the cache directory is given by \code{R_user_dir("caladaptr", "cache")}.
#' A custom location can be set with \code{ca_setcache}.
#'
#' @seealso \code{\link{ca_catalog_rs}}, \code{\link{ca_aoipreset_geom}}, \code{\link{ca_locagrid_geom}}
#'
#' @rawNamespace if (getRversion() >= "4.0.0") {importFrom(tools, R_user_dir) } else {importFrom(backports, R_user_dir)}
#' @import crayon
#' @export

ca_getcache <- function(quiet=TRUE) {

  cache_dir <- Sys.getenv("CALADAPTR_CACHE_DIR", unset = NA)

  if (is.na(cache_dir)) {

    ## No environment variable has been set (either by command or in .Renviron)
    default_cache_dir <- R_user_dir("caladaptr", "cache") %>% gsub("\\\\", "/", .)

    ## Use the default location (creating it if needed)
    if (file.exists(default_cache_dir)) {
      default_cache_dir  ## we're done here

    } else {
      dir.create(default_cache_dir, recursive = TRUE)

      ## Return the cache_dir if it was successfully created, else NA
      ifelse(file.exists(default_cache_dir), default_cache_dir, NA)
    }


  } else {
    if (!quiet) {
      msg_fmt <- getOption("ca_message", I)
      message(msg_fmt(paste0("Using cache directory: ", cache_dir)))
    }
    cache_dir
  }

}

#' @describeIn ca_getcache Set cache directory
#' @title Set cache directory to a custom location
#' @param cache_dir The directory for cached data
#' @param make_dir Make the directory if needed, logical
#' @param save Save the cache directory in the \code{.Renviron} file for persistence across R sessions
#' @param reset Change to the default location
#' @param quiet Suppress messages
#'
#' @rawNamespace if (getRversion() >= "4.0.0") {importFrom(tools, R_user_dir) } else {importFrom(backports, R_user_dir)}
#' @import crayon

ca_setcache <- function(cache_dir = NULL, make_dir = TRUE, save = TRUE, reset = FALSE, quiet = FALSE) {

  msg_fmt <- getOption("ca_message", I)

  if (reset) {
    cache_dir_use <- R_user_dir("caladaptr", "cache") %>% gsub("\\\\", "/", .)
    if (!quiet) message(msg_fmt(paste0(" - using default cache_dir: ", cache_dir_use)))

  } else if (is.null(cache_dir)) {   ## reset = FALSE, next check if cache_dir is NULL
    stop("cache_dir is required")

  } else {
    cache_dir_use <- cache_dir       ## reset = FALSE, use cache_dir
  }

  ## Create cache_dir_use if needed
  if (!file.exists(cache_dir_use)) {
    if (make_dir) dir.create(cache_dir_use, recursive = TRUE)

    if (!file.exists(cache_dir_use)) {
      stop(paste0(cache_dir_use, " can not be created. Please create it and try again."))
    }

  }

  ## Save the cache directory as an environment variable (in memory)
  Sys.setenv(CALADAPTR_CACHE_DIR = cache_dir_use)

  ## Save the cache directory to .Renviron
  if (save) {
    ## Grab the .Renviron file, creating it if needed
    environ_file <- file.path(Sys.getenv("HOME"), ".Renviron")
    if (!file.exists(environ_file)) file.create(environ_file)

    ## Read in .Renviron
    environ_lines <- readLines(environ_file)

    ## See if CALADAPTR_CACHE_DIR has already been written
    cachedir_idx <- grep("^CALADAPTR_CACHE_DIR=", environ_lines)

    if (length(cachedir_idx) == 0) {
      if (!quiet) message(msg_fmt(paste0(" - adding directory to ", environ_file)))
      environ_lines <- c(environ_lines, paste0("CALADAPTR_CACHE_DIR=", cache_dir_use))
      writeLines(environ_lines, environ_file)

    } else {
      if (!quiet) message(msg_fmt(paste0(" - updating cache directory in ", environ_file)))
      environ_lines[cachedir_idx] <- paste0("CALADAPTR_CACHE_DIR=", cache_dir_use)
      writeLines(environ_lines, environ_file)
    }

  }

}

