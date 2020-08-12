#' Creates a new blank API request object
#'
#' @export
#'

### ADD SOME ARGUMENTS SO THIS CAN BE SPECIFIED FROM SCRATCH

ca_apireq <- function() {
  res <- list(loc = NA,        ## list(type=..., val = ...)
              dates = NA,       ## list(start = start_dt, end = end_dt)
              dataset = NA,
              period = "day",
              cvar = NA,
              options = NA)
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




