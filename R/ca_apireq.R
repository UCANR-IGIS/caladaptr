#' Creates a new API request object
#'
#' Creates a new API request object
#'
#' @param loc A location object (see Details)
#' @param dates A dates object (see Details)
#' @param gcm A vector of GCM abbreviations (see `gcms`)
#' @param scenario A vector of scenario names (see `scenarios`)
#' @param period A vector of period names (see `periods`)
#' @param cvar A vector of climate variables (see `climvars`)
#' @param livneh Use Livneh dataset, logical
#' @param slug A vector of slugs
#' @param options A list of options for querying the API
#'
#' @export

ca_apireq <- function(loc = NA, dates = NA, gcm = NA, scenario = NA, period = NA, cvar = NA,
                      livneh = NA, slug = NA, options = NA) {
  res <- list(loc = loc,
              dates = dates,
              gcm = gcm,
              scenario = scenario,
              period = period,
              cvar = cvar,
              livneh = livneh,
              slug = slug,
              options = options)

  class(res) <- c("ca_apireq", "list")
  invisible(res)
}

# loc is a list object with two elements, 'type' and 'val'
# type will be one of: c("sf", "aoipreset", "zip", "pt")

# when type = "sf", val = list(loc = sf data frame, id = id (vector))

# when type = "aoipreset", val should be a list with THREE elements:
#  val = list(type = "counties",
#              idfld = "fips",
#              idvals = c("23425", "23425", "03824"))

# when type = "zip", val = a character vector of zip codes (US)

# when type = "pt", val = a 3-column data frame with columns id, x, y

# dates = list(start = start_dt, end = end_dt)


