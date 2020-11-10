#' Check the properties of a Cal-Adapt values databases
#'
#' @param x An object to be checked
#'
#' @details

#'
#' @seealso \code{\link{ca_getvals}}
#'
#' @importFrom DBI dbConnect dbDisconnect dbListTables dbReadTable dbSendQuery dbFetch dbClearResult dbListFields
#' @importFrom tibble glimpse
#' @importFrom RSQLite SQLite
#' @importFrom crayon yellow green bold red silver

#' @export

ca_check <- function(x) {

  if (inherits(x, "ca_apireq")) {

    ## Compute the number of locations
    if (x$loc$type == "aoipreset") {
      num_loc <- length(x$loc$val$idval)

    } else if (x$loc$type == "pt") {
      num_loc <- nrow(x$loc$val)

    } else {
      stop("Don't know yet how to compute the number of lcations")

    }

    ## Compute the number of dates
    start_dt <- as.Date(x$dates$start, format = "%Y-%m-%d")
    end_dt <- as.Date(x$dates$end, format = "%Y-%m-%d")

    if (x$period == "day") {
      num_dates <- as.numeric(difftime(end_dt, start_dt, units = "days")) + 1

    } else if (x$period == "month") {
      stop("Dont know how to deal with this period")

    } else if (x$period == "year") {
      stop("Dont know how to deal with this period")

    } else {
      stop("Dont know how to deal with this period")

    }

    ## Compute the number of slugs
    if (identical(x$slug, NA)) {
      num_slugs <- length(x$gcm) * length(x$scenario) * length(x$scenario) * length(x$cvar)

    } else {
      num_slugs <- length(x$slug)
    }

    ## Compute the number of spatial aggregation functions
    num_spag <- length(x$options$spatial_ag)


    num_vals_total <- num_loc * num_slugs * num_dates * num_spag
    message(green("Estimated number of values this API request will return: ", num_vals_total, sep = ""))

    message(yellow("TODO: check location extent against the rs extent"))
    message(yellow("TODO: verify units are all the same"))

    ## message(green("Estimated number of values this API request will return: ", num_vals, sep = ""))

  } else  if (is.character(x)) {

    ## We assume this is a SQLite database

    if (!file.exists(x)) stop("File does not exist")


    mydb <- dbConnect(SQLite(), x)

    ## Check the tables
    tbls <- dbListTables(mydb)

    if (length(tbls) > 0) {

      for (tbl in tbls) {

        cat(green("\nTABLE: ", toupper(tbl), sep = ""), "\n")

        # dbRead DOESN'T WORK WITH 'TRACTS_DAY_TASMAX'
        #glimpse(dbReadTable(mydb, tbl))

        cat("Fields #1:\n")
        print(DBI::dbListFields(mydb, tbl))
        cat("\n\n")

        cat("Fields 2:\n")
        res <- dbSendQuery(mydb, paste0("PRAGMA table_info(", tbl, ");"))
        print(dbFetch(res))
        dbClearResult(res)
        cat("\n\n")

        cat("Num Recs: \n")
        res <- dbSendQuery(mydb, paste0("SELECT COUNT(1) FROM ", tbl, ";"))
        print(dbFetch(res))
        dbClearResult(res)
        cat("\n\n")

        # cat("NOT SURE WHAT THIS IS \n")
        # res <- dbSendQuery(mydb, paste0("PRAGMA table_info(", tbl, ");"))
        # dbFetch(res)
        # dbClearResult(res)
        # cat("\n\n=====\n")

        # cat("Foreign Keys \n")
        # res <- dbSendQuery(mydb, "PRAGMA foreign_key_list(test_tbl);")
        # print(dbFetch(res))
        # dbClearResult(res)
        #
        # cat("INDICES (ALL) \n")
        # res <- dbSendQuery(mydb, paste0("PRAGMA index_list(", tbl, ");"))
        # dbFetch(res)
        # dbClearResult(res)

      }  # for tbl in tbls

    } # if length(tbls) > 0

    ## List all indices (this does not get all primary keys)
    cat("ALL INDICES (excluding primary keys): \n")
    res <- dbSendQuery(mydb, "SELECT * FROM sqlite_master WHERE type = 'index';")
    print(dbFetch(res))
    dbClearResult(res)

    dbDisconnect(mydb)

  } ## x is character


  invisible(NULL)

}

