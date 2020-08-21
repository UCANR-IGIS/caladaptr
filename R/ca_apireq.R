#' Creates a new API request object
#'
#' Creates a new API request object
#'
#' @param loc A location object (see Details)
#' @param dates A dates object (see Details)
#' @param gcm A vector of GCM abbreviations (see `gcms`)
#' @param scenario A vector of scenario names (see `scenarios`)
#' @param period A vector of period names (see periods)
#' @param cvar A vector of climate variables (see `climvars`)
#' @param options A list of options for querying the API
#'
#' @export
#'

ca_apireq <- function(loc=NA, dates=NA, gcm=NA, scenario=NA, period=NA, cvar=NA, options=NA) {
  res <- list(loc = loc,
              dates = dates,
              gcm = gcm,
              scenario = scenario,
              period = period,
              cvar = cvar,
              options = options)

  class(res) <- c("ca_apireq", "list")
  invisible(res)
}

# loc is a list object with two elements
# type = c("sf", "aoipreset", "zip", "pt")

# when type = "sf", val = a sf data frame

# when type = "aoipreset", val should be a list with three elements:
#  val = list(type = "counties",
#              idfld = "fips",
#              idvals = c("23425", "23425", "03824"))

# when type = "zip", val = a character vector of zip codes (US)

# when type = "pt", val = a 3-column data frame with columns id, x, y

# dates = list(start = start_dt, end = end_dt)


