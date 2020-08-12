#' Assigns a dataset to a Cal-Adapt API call
#'
#' Specifies a dataset to a Cal-Adapt API call
#'
#' @param x Cal-Adapt API request
#' @param gcm the name of a supported GCM
#' @param scenario the name of an emissions scenario
#'
#' @details
#' For valid options for \code{gcm} and \code{scenario}, see the
#' built-in constants \code{\link{gcms}} and \code{\link{scenarios}}.
#'
#' @export

ca_dataset <- function(x = ca_apireq(), gcm, scenario) {

  if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq")

  if (FALSE %in% (gcm %in% gcms)) {
    stop(paste0("Unknown value(s) in gcm: ",
                paste0(gcm[!gcm %in% gcms], collapse = ", " )))
  }

  if (FALSE %in% (scenario %in% scenarios)) {
    stop(paste0("Unknown value(s) in scenario: ",
                paste0(scenario[!scenario %in% scenarios], collapse = ", " )))
  }

  x$dataset <- list(gcm = gcm, scenario = scenario)

  invisible(x)

}
