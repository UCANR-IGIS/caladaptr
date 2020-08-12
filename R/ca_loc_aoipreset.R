#' Adds a preset location to a Cal-Adapt API request
#'
#' @param x A Cal-Adapt API request
#' @param type The type of AOI preset (see Details)
#' @param idfld The name of the field that identifies the desired locations
#' @param idval The value(s) of idfld
#' @param stat The summary statistic(s) to retrieve for the pixels that call within the AOI
#'
#' @details
#' \code{type} specifies one of the preset areas of interest supported by the Cal-Adapt API.
#' For valid values, view the built-in constant \code{aoipreset_types}.
#'
#' \code{idfld} is the field which contains the values you want to use to select the
#' preset areas. For a list of fields you can use for each AOI preset, see the built-in list
#' \code{ca_aoipreset_idflds}.
#'
#' \code{idval} are the value(s) you can use to select specific areas of interest. For a list
#' of values, see the built-in list \code{ca_aoipreset_idval}. If \code{idval = NULL}, all
#' areas will be used.
#'
#' \code(stat) is the name(s) of summary statistic(s) to compute from the pixels that fall within the
#' area of interest. Values can be \code{max}, \code{mean}, \code{median}, \code{min}, and \code{sum}.
#' Defaults to \code{mean}. To get retrieve raw values for all pixels without a summary, use
#' the \code{ca_getrst} function.
#'
#' @seealso \code{\link{ca_apireq}}
#' @export

ca_loc_aoipreset <- function(x = ca_apireq(), type, idfld="id", idval=NULL, stat = "mean") {

  if (!inherits(x, "ca_apireq")) stop("x should be an object of class ca_apireq")

  if (length(type) != 1) stop("type must be length 1")
  if (!type %in% aoipreset_types) stop("unknown value for type")
  if (FALSE %in% (idfld %in% aoipreset_idfld[[type]])) stop("invalid value for idfld. See aoipreset_idflds")

  if (!is.null(idval)) {
    if (FALSE %in% (idval %in% aoipreset_idval[[type]][[idfld]])) stop("invalid value(s) in idval. See aoipreset_idval")
  }

  if (length(stat) > 1) stop("Sorry, for right now you can only pass one stat function")

  if (FALSE %in% (stat %in% c("max", "mean", "median", "min", "sum"))) {
    stop(paste0("invalid value(s) in stat: ",
                paste(stat[!stat %in% c("max", "mean", "median", "min", "sum")], collapse = ", ")))
  }

  res <- x
  res$loc <- list(type="aoipreset", val = list(type = type,
                                               idfld = idfld,
                                               idval = idval,
                                               stat = stat))
  invisible(res)

}
