#' Add or delete indices
#'
#' @param x Either a remote tibble or a SQLite database file name
#' @param tbl The name of a table in the SQLite database
#' @param idx_fld_add Fields in tbl to create an index for
#' @param idx_fld_del Fields in tbl that have indices you'd like to delete
#' @param quiet Suppress messages
#'
#' @details
#' Database indices improve performance when you filter or sort rows based on field (column),
#' and/or join tables based on a common field. By default, indices are not created when you download
#' Cal-Adapt data into a SQLite database with \code{\link{ca_getvals_db}} (because they increase the
#' size of the SQLite file). You can tell \code{\link{ca_getvals_db}} to create indices with the \code{indices}
#' argument, or use \code{ca_db_indices} to create indices after data are downloaded.
#'
#' \code{x} can be either a remote tibble returned by \code{\link{ca_getvals_db}}, or a SQLite
#' database file name. \code{tbl} should be the name of a table in the database (i.e., the \code{db_tbl}
#' argument you passed to \code{ca_getvals_db}. If you're not sure what the table names are in a database,
#' run \code{\link{ca_db_info}}. Normally you would only add indices to a table that contains values
#' from Cal-Adapt (there is no need to add indices to lookup tables). You can only add indices for one
#' table at a time (but \code{idx_fld_add} can contain multiple field names).
#'
#' Note that \code{ca_db_indices} can only create indices on a single field. To create composite indices
#' you can run SQL expressions with the DBI package. Indices added by \code{ca_db_indices} will be named
#' automatically.
#'
#' For more details, see the vignette on querying large volumes of data:
#' \code{vignette("big-query", package = "caladaptr")}
#'
#' @seealso \code{\link{ca_getvals_db}}, \code{\link{ca_db_info}}
#'
#' @importFrom DBI dbConnect dbDisconnect dbListTables dbListFields dbGetQuery
#' @importFrom RSQLite SQLite
#' @import crayon
#'
#' @export

ca_db_indices <- function(x, tbl, idx_fld_add = NULL,
                        idx_fld_del = NULL, quiet = FALSE) {

  if (is.character(x)) {
    x_type <- "sqlite_fn"
    if (length(x) != 1) stop("Only one file name can be passed to this function")
    if (!file.exists(x)) stop(paste0(x, " not found"))
    db_conn = dbConnect(SQLite(), dbname = x)

  } else if (inherits(x, "ca_db")) {
    x_type <- "ca_db"
    db_conn = x$src$con

  } else {
    stop("x must be a remote tibble returned by ca_getvals_db(), or an SQLite database filename")

  }

  if (!tbl %in% dbListTables(db_conn)) stop(paste0(tbl, " not found in the database"))
  ##DBI::dbExistsTable()

  indices_added <- character(0)
  indices_deleted <- character(0)

  if (!quiet) {
    msg <- getOption("ca_message", paste0)
    success <- getOption("ca_success", paste0)
  }

  ## Verify the add fields are present
  tbl_flds <- dbListFields(db_conn, tbl)
  if (FALSE %in% (idx_fld_add %in% tbl_flds)) {
    stop(paste0("Field(s) not found in ", tbl, ": ",
                paste(setdiff(idx_fld_add, tbl_flds), collapse = ", ")))
  }

  ## Get all the fields already indexed (if any)
  indices_existing <- dbGetQuery(db_conn, paste0("PRAGMA index_list(`", tbl, "`);")) %>%
    pull(name) %>% as.character()

  for (fld in idx_fld_add) {
    index_name <- paste0(tbl, ".", fld)
    if (index_name %in% indices_existing) {
      if (!quiet) message(msg(paste0(index_name, " is already indexed, skipping")))
    } else {
      ## OK we need to add an index
      add_index_sql <- paste0("CREATE INDEX `", index_name, "` ON `", tbl, "`(`", fld, "`);")
      if (dbExecute(db_conn, add_index_sql) == 0) {
        indices_added <- c(indices_added, index_name)
        if (!quiet) message(success(paste0(index_name, " added")))
      }
    }
  }  ## for (fld in idx_fld_add)

  for (fld in idx_fld_del) {
    index_name <- paste0(tbl, ".", fld)
    if (!index_name %in% indices_existing) {
      if (!quiet) message(msg(paste0(index_name, " does not exist, skipping")))
    } else {
      ## OK we need to drop
      drop_index_sql <- paste0("DROP INDEX `", index_name, "`;")
      if (dbExecute(db_conn, drop_index_sql) == 0) {
        indices_deleted <- c(indices_deleted, index_name)
        if (!quiet) message(success(paste0(index_name, " dropped")))
      }
    }
  }  ## for (fld in idx_fld_del)

  #if (!is.null(idx_fld_del)) stop("Deleting indices is not yet supported")

  ## Disconnect if the user passed a filename. Otherwise, leave it open.
  if (x_type == "sqlite_fn") dbDisconnect(db_conn)

  ## Report results
  if (!quiet) {
    if (length(indices_added) == 0) {
      message(msg("No indices added"))
    } else {
      #message(success(paste0(length(indices_added), " indices added")))
    }

    if (length(indices_deleted) == 0) {
      message(msg("No indices deleted"))
    } else {
      #message(success(paste0(length(indices_deleted), " indices deleted")))
    }

  }

  invisible(x)

}
