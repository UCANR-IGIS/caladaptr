#' Assign climate variable(s) to a Cal-Adapt API call
#'
#' Specifies climate variable(s) a Cal-Adapt API call should retrieve
#'
#' @param x Cal-Adapt API request
#' @param cvar Climate variable
#'
#' @details
#' For valid options for \code{cvar}, see \code{\link{climvars}}.
#'
#' Notes:
#'
#' 1) 'climate variables' refers to both the variables returned by global
#' circulation models (e.g., temperature, precipitation), as well as
#' variables derived by additional models (e.g., evapo-transpiration)
#'
#' 2) Not all climate variables are available for all climate models,
#' temporal periods, and date ranges.
#'
#' @export

ca_cvar <- function(x = ca_apireq(), cvar) {

  if (!inherits(x, "ca_apireq")) stop("x should be an object of class ca_apireq")

  if (FALSE %in% (cvar %in% climvars)) {
    stop(paste0("Unknown value(s) in period: ",
                paste0(cvar[!cvar %in% climvars], collapse = ", " ), ". Run `climvars` for valid values."))
  }

  x$cvar <- cvar
  invisible(x)

}

