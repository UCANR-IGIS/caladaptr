#' Run checks on an API request object
#'
#' Run checks on an API request object
#'
#' @param x A Cal-Adapt API request
#' @param slug_check Cross check the slug against the raster series catalog
#' @param date_check Cross check the start and end date against the raster series catalog
#' @param loc_check Check to make sure the location is within the Cal-Adapt coverage area
#' @param units_check Check for consistent units
#' @param spag_check Check spatial aggregation option
#' @param check_for What to check for - getting values or getting rasters
#' @param quiet Suppress messages
#' @param ignore_spag Deprecated
#'
#' @details This function checks an Cal-Adapt API request for potential problems. It checks to make sure
#' the request:
#'
#' \itemize{
#'    \item is complete
#'    \item doesn't have conflicting elements
#'    \item specifies an existing datasets
#'    \item specifies a location within the Cal-Adapt coverage area
#'    \item specifies area-of-interest presets correctly
#'    \item has features that are not too large for the Cal-Adapt API
#'    \item doesn't mix datasets that have different units
#'    \item uses dates that fall within the Cal-Adapt time series
#'    \item includes a spatial aggregation function if needed
#' }
#'
#' Most of the checks can be selectively disabled using arguments. \code{check_for} allows you to tailor checks for
#' querying values and/or downloading rasters.
#'
#' @returns TRUE if no messages are reported, else FALSE
#'
#' @seealso \code{\link{ca_catalog_rs}}
#'
#' @importFrom lifecycle is_present deprecate_warn deprecated
#'
#' @export

ca_preflight <- function(x, slug_check = TRUE, date_check = TRUE, loc_check = TRUE,
                         units_check = TRUE, spag_check = TRUE, check_for = c("getvals", "getrst"), quiet = FALSE,
                         ignore_spag = deprecated()) {

  if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq")
  if (FALSE %in% (check_for %in% c("getvals", "getrst"))) stop("unknown value(s) for `check_for`")

  if (is_present(ignore_spag)) {
    deprecate_warn("0.6.0", "ca_preflight(ignore_spag)", "ca_preflight(spag_check)")
    spag_check <- !ignore_spag
  }

  msgs_lst <- ca_apicalls(x,
                      slug_check = slug_check,
                      date_check = date_check,
                      loc_check = loc_check,
                      units_check = units_check,
                      spag_check = spag_check,
                      check_for = check_for,
                      pf = TRUE)

  if (!quiet) {

    ## Get the function(s) we'll use to style messages
    accent1 <- if (interactive()) getOption("ca_accent1", I) else I
    accent2 <- if (interactive()) getOption("ca_accent2", I) else I
    success_fmt <- if (interactive()) getOption("ca_success", I) else I

    msg_chr <- accent1("General issues")
    if (length(msgs_lst$genl_msgs) > 0) {
      for (msg_one in msgs_lst$genl_msgs) {
        msg_chr <- paste0(msg_chr, "\n", accent2(paste0(" - ", msg_one)))
      }
    } else {
      msg_chr <- paste0(msg_chr, "\n", success_fmt(paste0(" - none found")))
    }

    if ("getvals" %in% check_for) {
      msg_chr <- paste0(msg_chr, "\n", accent1("Issues for querying values"))
      if (length(msgs_lst$gval_msgs) > 0) {
        for (msg_one in msgs_lst$gval_msgs) {
          msg_chr <- paste0(msg_chr, "\n", accent2(paste0(" - ", msg_one)))
        }
      } else {
        msg_chr <- paste0(msg_chr, "\n", success_fmt(paste0(" - none found")))
      }
    }

    if ("getrst" %in% check_for) {
      msg_chr <- paste0(msg_chr, "\n", accent1("Issues for downloading rasters"))
      if (length(msgs_lst$grst_msgs) > 0) {
        for (msg_one in msgs_lst$grst_msgs) {
          msg_chr <- paste0(msg_chr, "\n", accent2(paste0(" - ", msg_one)))
        }
      } else {
        msg_chr <- paste0(msg_chr, "\n", success_fmt(paste0(" - none found")))
      }
    }

    message(msg_chr)

  }

  if (length(unlist(msgs_lst)) == 0) {
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }

}

