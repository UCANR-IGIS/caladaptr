#' Read a Cal-Adapt values database SQLite file
#'
#' @param x A Cal-Adapt values remote tibble or SQLite file name
#' @param val_tbl The name of table which contains climate data, ignored if all_tables = TRUE
#' @param join_lookup_tbls Join lookup tables if present in the remote tibble
#' @param all_tables Return all tables, logical
#' @param exclude_hash_tables Exclude tables that contain search hashes, ignored if all_tables = FALSE
#'
#' @details This will 'mount' a SQLite database created by \code{\link{ca_getvals_db}}, and return a remote
#' tibble (i.e., a tibble connected to a database). \code{x} can be either a SQLite file name or a
#' remote tibble returned by \code{\link{ca_getvals_db}}.
#'
#' \code{val_tbl} should be the name of the table in the database that contains the climate values (i.e., the
#' same table you specified when you ran \code{\link{ca_getvals_db}}). If \code{val_tbl = NULL} and a sidecar
#' text file for the SQLite database exists, it will search the sidecar file for the name of a values table and
#' use the first one. if \code{join_lookup_tbls = TRUE} and lookup tables were used when fetching the data,
#' the remote tibble returned will be based on a SQL expression that joins the lookup tables to the values table.
#' Joining lookup tables is only possible if a sidecar text file exists.
#'
#' If \code{all_tables = TRUE}, a list of remote tibbles for all the tables in the database will be returned. In this case,
#' \code{val_tbl} and \code{join_lookup_tbls} are ignored. If \code{exclude_hash_tables = TRUE}, tables that store search
#' hashes will be excluded in the returned list.
#'
#' @return
#' A remote tibble with climate values if all_tables = FALSE, otherwise a list of remote tibbles if all_tables = TRUE
#'
#' @seealso \code{\link{ca_getvals_db}}, \code{\link{ca_db_info}}
#'
#' @importFrom DBI dbConnect dbListTables
#' @importFrom RSQLite SQLite
#' @importFrom dplyr tbl
#' @export

#select mutate left_join tbl sql bind_rows

ca_read_db <- function(x, val_tbl = NULL, join_lookup_tbls = TRUE, all_tables = FALSE, exclude_hash_tables = TRUE) {


  if (is.character(x)) {
    if (length(x) != 1) stop("Only one file name can be passed to this function")
    if (!file.exists(x)) stop(paste0(x, " not found"))
    sqlite_fn <- x

  } else if (inherits(x, "ca_db")) {
    sqlite_fn <- x$src$con

  } else {
    stop("x must be a remote tibble returned by ca_getvals_db(), or an SQLite database filename")

  }

  # if (!is.character(x)) stop("x should be a SQLite file name")
  # if (!file.exists(x)) stop(paste0(x, " not found"))

  ## if all_tables - return a list of all the tables. Default false. Otherwise only return val_tbl.

  ## if join_tables, return the main values table only
  ##  if no sidecar file is found, return an error
  ##  if val_tbl is not found in the sidecar, return an error

  ## accnt1 <- getOption("ca_accent1", paste0)

  ## Open the database
  db_conn = dbConnect(SQLite(), dbname = sqlite_fn)

  ## Get the tables
  tbls <- dbListTables(db_conn)

  if (all_tables) {

    ## Return a list object with all the tables

    ## Omit '_hashes' tables
    if (exclude_hash_tables) {
      tbls <- tbls[!grepl("_hashes$", tbls)]
    }

    ## Add each table to the result list
    res <- list()
    for (mytbl in tbls) {
      res[[mytbl]] <- tbl(db_conn, mytbl)
    }

  } else {

    ## Want just the values table

    if (!is.null(val_tbl) && !join_lookup_tbls) {
      ## The user provided the name of a table, and they don't want to join any lookup tables
      ## so if the table exists we can just return it.

      if (val_tbl %in% tbls) {
        res <- tbl(db_conn, val_tbl)
      } else {
        stop(paste0("Table '", val_tbl, "' not found in this database"))
      }

    } else {

      join_sql <- NULL

      ## We need loop through the side car

      ## Look for a sidecar file
      sc_fn <- paste0(x, ".txt")

      if (file.exists(sc_fn)) {

        fcon <- file(sc_fn, open = "r")

        ## Loop thru the lines
        while ( TRUE ) {
          one_line <- readLines(fcon, n = 1, warn = FALSE)

          ## If this is the last line, stop
          if (length(one_line) == 0 ) break

          ## Determine if this line is comment
          first_char <- trimws(substr(one_line, 1, 1))
          if (first_char != "#" && first_char != "/") {

            ## Look for a colon
            colon_pos <- regexpr(":", one_line)
            if (colon_pos > 0) {

              ## Get the key and value
              ln_key <- trimws(substring(one_line, 1, colon_pos - 1)[1])
              ln_val <- gsub("\"", "'", trimws(substring(one_line, colon_pos + 1)[1]))

              if (ln_key == "val_tbl") {
                if (is.null(val_tbl)) {
                  val_tbl <- ln_val
                  #message(paste0("Just set val_tbl to ", val_tbl))
                  if (!join_lookup_tbls) break
                }
              }

              if (!is.null(val_tbl) && join_lookup_tbls) {
                # if (tolower(ln_key) == tolower(paste0(val_tbl, "_lkp"))) {
                #   message(paste0("Just got the lookup status for the desired table: ", ln_val))
                # }

                if (tolower(ln_key) == tolower(paste0(val_tbl, "_sql"))) {
                  join_sql <- ln_val
                  #message(paste0("Just got the SQL for ", val_tbl, ": ", join_sql))
                  break
                }

              }

            }

          }

        }  ## while TRUE
        close(fcon)

      } ## if  sidecar file found

      if (is.null(val_tbl)) {
        ## The user didn't provide the name of a values table, and one was not found in the sidecar file.
        stop("no values table not found")
      }

      if (is.null(join_sql)) {
        ## We didn't find (or didn't look for) a SQL phrase. So just return the values table by itself.
        if (val_tbl %in% tbls) {
          res <- tbl(db_conn, val_tbl)
        } else {
          stop(paste0(val_tbl, " not found in this database"))
        }

      } else {#
        res <- tbl(db_conn, sql(join_sql))

      }

      class(res) <- c(class(res), "ca_db")
      #attr(res, "lkp_sql") <- tbl_sql   these have to go in xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      #attr(res, "vals_tbl") <- db_tbl


    }

  }

  ## Disconnect
  # message("going to disconnect")
  # dbDisconnect(db_conn)

  ## Return
  invisible(res)

}
