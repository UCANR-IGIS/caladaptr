#' Format a ca_apireq object
#'
#' @param x Cal-Adapt API request
#' @importFrom crayon yellow bold
#' @importFrom sf st_geometry_type
#' @export
#' @method format ca_apireq

format.ca_apireq <- function(x) {

  loc1 <- yellow("Location(s): ")
  if (identical(x$loc, NA)) {
    loc2 <- "NA"
  } else if (is.list(x$loc)) {

    if (x$loc$type == "sf") {
      loc2 <- paste0(as.character(unique(st_geometry_type(x$loc$val))), " (", nrow(x$loc$val), " features)")

    } else if (x$loc$type == "aoipreset") {

      ## Construct the list of idvals
      if (is.null(x$loc$val$idval)) {
        idvals_str <- "all"
      } else {
        idvals_str <- paste0(paste(head(x$loc$val$idval, n=3), collapse=", "),
                             ifelse(nrow(x$loc$val$idval)>3, ", ...", ""))
      }
      loc2 <- paste0("\n  AOI Preset: ", x$loc$val$type,
                     "\n  ", x$loc$val$idfld, "(s): ", idvals_str)

    } else if (x$loc$type == "zip") {
      loc2 <- paste0("\n  ", x$loc$val$type, "\n  ", x$loc$val$idfld, ": ",
                    paste(x$loc$val$idval, collapse=", "))

    } else if (x$loc$type == "pt") {
      if (nrow(x$loc$val) > 3) {
        str_dotdotdot <- ", ..."
      } else {
        str_dotdotdot <- ""
      }

      pts3 <- round(as.matrix(head(x$loc$val[, c(2,3)], n=3)), 3)
      loc2 <- paste0("\n  x: ", paste(pts3[,1], collapse=", "), str_dotdotdot,
                     "\n  y: ", paste(pts3[,2], collapse=", "), str_dotdotdot)

    } else {
      loc2 <- "unknown location type"
    }
  }
  loc_str <- paste0(loc1, loc2, "\n")

  vars_str <- paste0(yellow("Variable(s): "), paste(x$cvar, collapse = ", "), "\n")

  gcm_str <- paste0(yellow("GCM(s): "), paste(x$gcm, collapse = ", "), "\n")

  scen_str <- paste0(yellow("Scenario(s): "), paste(x$scenario, collapse = ", "), "\n")

  per_str <- paste0(yellow("Temporal aggregration period(s): "), paste(x$period, collapse = ", "), "\n")

  if (identical(x$dates, NA)) {
    dates_val <- "NA"
  } else {
    dates_val <- paste(x$dates$start, "to", x$dates$end)
  }
  dates_str <- paste0(yellow("Dates: "), dates_val, "\n")

  if (identical(x$options, NA)) {
    opt_obj <- "NA"
  } else {
    opt_obj <- paste0("\n  spatial ag: ", paste(x$options$spatial_ag, collapse = ", "),
                      "\n  temporal ag (add'l): ", x$options$temporal_ag)
  }
  options_str <- paste0(yellow("Options: "), opt_obj, "\n")

  invisible(paste0(loc_str, vars_str, per_str, gcm_str, scen_str, dates_str, options_str))

}

#' Print a ca_apireq object
#'
#' @param x Cal-Adapt API request
#' @importFrom crayon yellow bold
#' @method print ca_apireq
#' @export

print.ca_apireq <- function(x) {
  cat(yellow$bold("Cal-Adapt API Request\n"))
  cat(format(x), "\n")
}

