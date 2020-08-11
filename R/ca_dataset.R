#' Specifies the location based on a sf object
#'
#' @export

ca_dataset <- function(x, ds="scripps_loca", gcm, sc) {

  if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq")

  message("Check value for resource here")
  if (FALSE %in% (gcm %in% ca_gcm)) stop("unknown value in gcm")
  if (FALSE %in% (sc %in% ca_scenario)) stop("unknown value in gcm")

  x$resource <- list(ds = ds,
                     gcm = gcm,
                     sc = sc)

  invisible(x)

}
