#' Creates a new blank API request object
#'
#' @export
#'

### ADD SOME ARGUMENTS SO THIS CAN BE SPECIFIED FROM SCRATCH

ca_apireq <- function() {
  res <- list(loc = NA,        ## list(type="sf", val = loc)
              dates = NA,       ## list(start = start_dt, end = end_dt)
              dataset = NA,
              cvar = NA,
              options = NA)
  class(res) <- c("ca_apireq", "list")
  invisible(res)
}

# loc is a list object with two elements
# type = c("sf", "aoipreset", "zip", "pt")
# type = "sf"
#  val = a sf data frame
#
# type = "aoipreset"
#  val = list(type = "counties",
#              idfld = "fips",
#              idvals = c("23425", "23425", "03824"))





