#' Format a ca_apireq object
#'
#' @importFrom crayon yellow bold
#' @export

format.ca_apireq <- function(x) {

  # loc_str <- paste0(crayon::yellow("Location: "),
  #                    if (identical(x$loc, NA)) {
  #                       "NA\n"
  #                     } else {
  #                       paste0("\n  type: ", x$loc$type,
  #                              "\n  value: ", x$loc$val, "\n")
  #                     })

  loc1 <- yellow("Location: ")
  if (identical(x$loc, NA)) {
    loc2 <- "NA"
  } else if (is.list(x$loc)) {
    if (x$loc$type == "sf") {
      loc2 <- paste0(as.character(unique(st_geometry_type(x$loc$val))), " (", nrow(x$loc$val), " features)")
    } else if (x$loc$type == "aoipreset") {
      loc2 <- paste0("\n  AOI Preset: ", x$loc$val$type, "\n  ", x$loc$val$idfld, "(s): ",
                    paste(x$loc$val$idval, collapse=", "))
    } else if (x$loc$type == "zip") {
      loc2 <- paste0("\n  ", x$loc$val$type, "\n  ", x$loc$val$idfld, ": ",
                    paste(x$loc$val$idval, collapse=", "))
    } else {
      loc2 <- "unknown location type"
    }
  }
  loc_str <- paste0(loc1, loc2, "\n")

  ## This needs work for multiple GCMs and SC
  data_str <- paste0(crayon::yellow("Dataset: "), paste(x$dataset, collapse=", "), "\n")

  vars_str <- paste0(crayon::yellow("Variable(s): "), paste(x$cvar, collapse = ", "), "\n")

  dates_str <- paste0(crayon::yellow("Dates: "), ifelse(identical(x$dates, NA), "NA", paste(x$dates$start, "to", x$dates$end)), "\n")
  options_str <- paste0(crayon::yellow("Options: "), x$options)

  invisible(paste0(loc_str, data_str, dates_str, vars_str, options_str))

  #tempag_str <- paste0(crayon::yellow("Temporal aggregation: "), paste(x$tempag, collapse = ", "), "\n")
  #res <- list(geom = NA, dates = NA, resource = NA, cvar = NA, tempag = NA, options = NA)

}

#' Print a ca_apireq object
#'
#' @importFrom crayon yellow bold
#' @export

print.ca_apireq <- function(x) {
  cat(crayon::yellow$bold("Cal-Adapt API Request\n"))
  cat(format(x), "\n")
}

