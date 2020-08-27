#' Assign emission scenario(s) to a Cal-Adapt API call
#'
#' Specifies emission scenario(s) for retrieval
#'
#' @param x Cal-Adapt API request
#' @param scenario Abbreviation of emissions scenario(s)
#'
#' @details
#' For valid options for \code{scenario}, see \code{\link{scenarios}}.
#'
#' @export

ca_scenario <- function(x = ca_apireq(), scenario) {

  if (!inherits(x, "ca_apireq")) stop("x should be an object of class ca_apireq")

  if (!identical(scenario, NA)) {
    if (FALSE %in% (scenario %in% scenarios)) {
      stop(paste0("Unknown value(s) in scenario: ",
                  paste0(scenario[!scenario %in% scenarios], collapse = ", " ), ". Run `scenarios` for valid values."))
    }
  }

  x$scenario <- scenario
  invisible(x)

}

