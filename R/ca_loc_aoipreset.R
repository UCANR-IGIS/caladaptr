#' Adds a preset location to a Cal-Adapt API request
#'
#' @param x A Cal-Adapt API request
#' @param type The type of AOI preset (see Details)
#' @param idfld The name of the field that identifies the desired locations
#' @param idval The value(s) of idfld
#'
#' @details
#' \code{type} specifies one of the preset areas of interest supported by the Cal-Adapt API.
#' For valid values, view the built-in constant \code{ca_aoipreset}.
#'
#' \code{idfld} is the field which contains the values you want to use to select the
#' preset areas. For a list of fields you can use for each AOI preset, see the built-in list
#' \code{ca_aoipreset_idfld}.
#'
#' \code{idval} are the value(s) you can use to select specific areas of interest. For a list
#' of values, see the builtin list \code{ca_aoipreset_idval}. If \code{idval = NULL}, all
#' areas will be used.
#'
#' @seealso \code{\link{ca_apireq}}
#' @export

ca_loc_aoipreset <- function(x = ca_apireq(), type, idfld="id", idval=NULL) {

  if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq")

  if (length(type) != 1) stop("type must be length 1")
  if (!type %in% ca_aoipreset_type) stop("unknown value for type")
  if (FALSE %in% (idfld %in% ca_aoipreset_idfld[[type]])) stop("invalid value for idfld. See ca_aoipreset_idfld.")

  if (!is.null(idval)) {
    if (FALSE %in% (idval %in% ca_aoipreset_idval[[type]][[idfld]])) stop("invalid value in idval. See ca_aoipreset_idval.")
  }

  res <- x
  res$loc <- list(type="aoipreset", val = list(type = type,
                                               idfld = idfld,
                                               idval = idval))
  invisible(res)

}
