#' Format a pair of dates for URLs
#'
#' Format dates to append to an API End Point

## DOES THIS NEED TO BE EXPORTED??

ca_fmtdate <- function(date1, date2=NULL) {

  if (is.null(date1)) return(NULL)

  if (length(date1)==2) {
    date1use <- date1[1]
    date2use <- date1[2]
  } else {
    date1use <- date1
    date2use <- date2
  }

  if (date1use >= date2use) stop("The begin date should be earlier than the end date")

  paste0(format(date1use, "%Y-%m-%d"), "/", format(date2use, "%Y-%m-%d"), "/")

}

