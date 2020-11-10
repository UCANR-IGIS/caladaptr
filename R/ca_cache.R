#' Manage Cache Directory
#'
#' View and set the directory for the data catalog
#'
#' @param default Use a default cache directory if no other has been set
#' @param quiet Show messages, logical
#'
#' @details \code{caladaptr} has the ability to store copies of objects downloaded
#' from Cal-Adapt. An example of this would be the raster series data catalog and the
#' geometries of AOI Presets.
#'
#' NOTE: \code{caladaptr} does \emph{not} cache climate data fetched from Cal-Adapt. Every time
#' you call a function that fetches data (e.g., \code{\link{ca_getvals}}), data is retrieved fresh.
#'
#' If \code{default = TRUE}, a default cache directory will be used (\link[rappdirs]{user_cache_dir})
#' \emph{if} one hasn't already been set with \code{ca_setcache}.
#'
#' @seealso \code{\link{ca_catalog_rs}}
#'
#' @importFrom rappdirs user_cache_dir
#' @export

ca_getcache <- function(default=TRUE, quiet=TRUE) {

  cache_dir <- Sys.getenv("CALADAPTR_CACHE_DIR", unset = NA)

  if (is.na(cache_dir)) {

    ## No environment variable has been set (either by command or in .Renviron)
    if (default) {

      ## Use rappdir to find the default location for a cache dir
      default_cache_dir <- user_cache_dir("caladaptr", "ucanr-igis")

      ## Use the default location (creating it if needed)
      if (file.exists(default_cache_dir)) {
        default_cache_dir
      } else {
        dir.create(default_cache_dir, recursive = TRUE)
        ## Return the cache_dir if it was successfully created, else NA
        ifelse(file.exists(default_cache_dir), default_cache_dir, NA)
      }

      # DEPRECATED:
      #default_dir <- "~/.R/caladaptr"
      # if (file.exists(default_dir)) {
      #   if (file.info(default_dir)$isdir) {
      #     ## We're done
      #     if (!quiet) message(paste0("using cache directory: ", default_dir))
      #     default_dir
      #   } else {
      #     ## Default dir exists, but is not a directory
      #     warning(paste0(default_dir, " exists but is not a directory"))
      #     NA
      #   }
      # } else {
      #   ## Default dir doesn't exist, attempt to create it
      #   made_dir <- TRUE
      #   if (!file.exists("~/.R")) {
      #     made_dir <- made_dir && dir.create("~/.R")
      #   }
      #   if (made_dir && !file.exists("~/.R/caladaptr")) {
      #     made_dir <- made_dir && dir.create("~/.R/caladaptr")
      #   }
      #
      #   ## Return the default dir or NA
      #   if (made_dir) {
      #     default_dir
      #   } else {
      #     NA
      #   }
      #
      # }

    } else {
      if (!quiet) message("A cache directory could not be found. Use default = TRUE or run ca_setcache.")
      NA
    }

  } else {
    if (!quiet) message(paste0("using cache directory: ", cache_dir))
    cache_dir
  }

}

#' @describeIn ca_getcache Set cache directory
#' @title Set cache directory to a custom location
#' @param cache_dir The directory for cached data
#' @param make_dir Make the directory if needed, logical
#' @param write Save the cache directory in the \code{.Renviron} file for persistence across R sessions
#' @param reset Change to the default location
#' @param quiet Suppress messages
#'
#' @importFrom rappdirs user_cache_dir
#' @importFrom crayon yellow

ca_setcache <- function(cache_dir = NULL, make_dir = FALSE, write = TRUE, reset = FALSE, quiet = FALSE) {

  if (reset) {
    cache_dir_use <- user_cache_dir("caladaptr", "ucanr-igis")
    if (!quiet) message(yellow(paste0(" - using default cache_dir: ", cache_dir_use)))
  } else if (is.null(cache_dir)) {
    stop("cache_dir is required")
  } else {
    cache_dir_use <- cache_dir
  }

  if (!file.exists(cache_dir_use)) {
    if (make_dir) dir.create(cache_dir_use, recursive = TRUE)

    if (!file.exists(cache_dir_use)) {
      stop(paste0(cache_dir_use, " does not exist. Please create it and try again."))
    }

    ## If they passed the default, attempt to create it
    # if (dir == "~/.R/caladaptr") {
    #   made_dir <- TRUE
    #   if (!file.exists("~/.R")) {
    #     made_dir <- made_dir && dir.create("~/.R")
    #   }
    #   if (made_dir && !file.exists("~/.R/caladaptr")) {
    #     made_dir <- made_dir && dir.create("~/.R/caladaptr")
    #   }
    #   if (!made_dir) stop("Can not create cache directory. Try passing a different location for dir.")
    # }
  }

  ## Save the cache directory as an environment variable (in memory)
  Sys.setenv(CALADAPTR_CACHE_DIR = cache_dir_use)

  ## Save the cache directory to .Renviron
  if (write) {
    ## Grab the .Renviron file, creating it if needed
    environ_file <- file.path(Sys.getenv("HOME"), ".Renviron")
    if (!file.exists(environ_file)) file.create(environ_file)

    ## Read in .Renviron
    environ_lines <- readLines(environ_file)

    ## See if CALADAPTR_CACHE_DIR has already been written
    cachedir_idx <- grep("^CALADAPTR_CACHE_DIR=", environ_lines)

    if (length(cachedir_idx) == 0) {
      if (!quiet) message(yellow(paste0(" - adding directory to ", environ_file)))
      environ_lines <- c(environ_lines, paste0("CALADAPTR_CACHE_DIR=", cache_dir_use))
      writeLines(environ_lines, environ_file)

    } else {
      if (!quiet) message(yellow(paste0(" - updating cache directory in ", environ_file)))
      environ_lines[cachedir_idx] <- paste0("CALADAPTR_CACHE_DIR=", cache_dir_use)
      writeLines(environ_lines, environ_file)
    }

  }

}

