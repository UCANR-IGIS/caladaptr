#' Manage Cache Directory
#'
#' View and set the directory for the data catalog
#'
#' @param default Use a default cache directory if no other has been set
#' @param quiet Show messages, logical
#'
#' @details \code{caladaptr} has the ability to store copies of objects downloaded
#' from Cal-Adapt. An example of this would be the raster series data catalog.
#'
#' If \code{default = TRUE}, a default directory for the cache (\emph{~/.R/caladaptr})
#' will be used if another one has not already been set.
#'
#' @seealso \code{\link{ca_catalog_rs}}
#'
#' @export

ca_getcache <- function(default=TRUE, quiet=TRUE) {

  cache_dir <- Sys.getenv("CALADAPTR_CACHE_DIR", unset = NA)

  if (is.na(cache_dir)) {

    ## No environment variable has been set (either by command or in .Renviron)
    if (default) {

      ## Use the default location (creating it if needed)
      default_dir <- "~/.R/caladaptr"
      if (file.exists(default_dir)) {
        if (file.info(default_dir)$isdir) {
          ## We're done
          if (!quiet) message(paste0("using cache directory: ", default_dir))
          default_dir
        } else {
          ## Default dir exists, but is not a directory
          warning(paste0(default_dir, " exists but is not a directory"))
          NA
        }
      } else {
        ## Default dir doesn't exist, attempt to create it
        made_dir <- TRUE
        if (!file.exists("~/.R")) {
          made_dir <- made_dir && dir.create("~/.R")
        }
        if (made_dir && !file.exists("~/.R/caladaptr")) {
          made_dir <- made_dir && dir.create("~/.R/caladaptr")
        }

        ## Return the default dir or NA
        if (made_dir) {
          default_dir
        } else {
          NA
        }

      }

    } else {
      if (!quiet) {
        message("A cache directory could not be found. Use default = TRUE or run ca_setcache.")
      }
      NA
    }


  } else {
    if (!quiet) message(paste0("using cache directory: ", cache_dir))
    cache_dir
  }

}

#' @describeIn ca_getcache Set cache directory
#' @title Set cache directory
#' @param dir The directory for  cached EXIF data (must exist)
#' @param write Write directory location to .Renviron
#' @param quiet Suppress messages

ca_setcache <- function(dir = "~/.R/caladaptr", write = TRUE, quiet = FALSE) {

  if (!file.exists(dir)) {
    ## If they passed the default, attempt to create it
    if (dir == "~/.R/caladaptr") {
      made_dir <- TRUE
      if (!file.exists("~/.R")) {
        made_dir <- made_dir && dir.create("~/.R")
      }
      if (made_dir && !file.exists("~/.R/caladaptr")) {
        made_dir <- made_dir && dir.create("~/.R/caladaptr")
      }
      if (!made_dir) stop("Can not create cache directory. Try passing a different location for dir.")
    }

    stop(paste0(dir, " does not exist. Please create it and try again."))
  }

  ## Save the cache directory as an environment variable (in memory)
  Sys.setenv(CALADAPTR_CACHE_DIR = dir)

  if (write) {
    ## Grab the .Renviron file, creating it if needed
    environ_file <- file.path(Sys.getenv("HOME"), ".Renviron")
    if (!file.exists(environ_file)) {
      file.create(environ_file)
    }

    ## Read in .Renviron
    environ_lines <- readLines(environ_file)

    ## See if CALADAPTR_CACHE_DIR has already been saved
    cachedir_idx <- grep("^CALADAPTR_CACHE_DIR=", environ_lines)

    if (length(cachedir_idx) == 0) {
      if (!quiet) message(paste0("Adding directory to ", environ_file))
      environ_lines <- c(environ_lines, paste0("CALADAPTR_CACHE_DIR=", dir))
      writeLines(environ_lines, environ_file)

    } else {
      if (!quiet) message(paste0("Updating cache directory in ", environ_file))
      environ_lines[cachedir_idx] <- paste0("CALADAPTR_CACHE_DIR=", dir)
      writeLines(environ_lines, environ_file)
    }


  }

}

