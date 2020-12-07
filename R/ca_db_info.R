#' Check the properties of a Cal-Adapt values databases
#'
#' @param x Either a remote tibble or a SQLite database file name
#'
#' @details
#' \code{x} can be either a remote tibble returned by \code{\link{ca_getvals_db}}, or a SQLite
#' database file name.
#'
#' @return
#' A list object with info about \code{x}, including the tables, fields, and indices.
#'
#' @seealso \code{\link{ca_getvals_db}}
#'
#' @importFrom DBI dbConnect dbDisconnect dbListTables dbGetQuery dbListFields
#' @importFrom RSQLite SQLite
#' @importFrom dbplyr sql_render
#' @export

ca_db_info <- function(x) {

  res <- list()

  if (is.character(x)) {
    x_type <- "sqlite_fn"
    if (length(x) != 1) stop("Only one file name can be passed to this function")
    if (!file.exists(x)) stop(paste0(x, " not found"))
    db_conn = dbConnect(SQLite(), dbname = x)
    res$sql <- NA
    res$fn <- x

  } else if (inherits(x, "ca_db")) {
    x_type <- "ca_db"
    db_conn = x$src$con
    res$sql <- sql_render(x)    ## class 'sql', 'character'
    res$fn <- db_conn@dbname %>% gsub("\\\\", "/", .)

  } else {
    stop("x must be a remote tibble returned by ca_getvals_db(), or an SQLite database filename")

  }

  accnt1 <- getOption("ca_accent1", paste0)

  ## Get the tables
  tbls <- dbListTables(db_conn)

  ## Put the tables in groups
  tbl_grps_lst <- list()

  ## FIX this will fail if no hashes are recorded
  tbl_grps_lst$hash_tbl <- grep("_hashes$", tbls)
  tbl_grps_lst$val_tbl <- as.integer(sapply(tbl_grps_lst$hash_tbl,
                                 function(i) grep(paste0("^", gsub("_hashes$", "", tbls[i]), "$"), tbls)))
  valid_lkp_tbls <- c("cvars", "gcms", "scenarios", "periods", "spags", "slugs")
  tbl_grps_lst$lkp_tbl <- which(tbls %in% intersect(tbls, valid_lkp_tbls))
  tbl_grps_lst$other_tbl <- setdiff(1:length(tbls),
                            c(tbl_grps_lst$hash_tbl, tbl_grps_lst$val_tbl, tbl_grps_lst$lkp_tbl))

  ## Identify all the hashes tables
  ## idx_hash_tbls <- grep("_hashes$", tbls)

  ## Identify all the values tables
  # idx_vals_tbls <- sapply(idx_hash_tbls,
  #                         function(i) grep(paste0("^", gsub("_hashes$", "", tbls[i]), "$"), tbls))

  ## Identify all the lookup tables
  # valid_lkp_tbls <- c("cvars", "gcms", "scenarios", "periods", "spags", "slugs")
  # idx_lkp_tbls <- which(tbls %in% intersect(tbls, valid_lkp_tbls))

  ## Identify everything else
  # idx_other_tbls <- setdiff(1:length(tbls),
  #                           c(idx_hash_tbls, idx_vals_tbls, idx_lkp_tbls))

  ## Record details for each table
  res$tbls <- list()

  for (tbl_grp in c("val_tbl", "lkp_tbl", "hash_tbl", "other_tbl")) {

    ## Create an element for these tables
    res$tbls[[tbl_grp]] <- list()

    for (tbl in tbls[tbl_grps_lst[[ tbl_grp]]]) {

      num_rows <- dbGetQuery(db_conn, paste0("SELECT COUNT(1) FROM `", tbl, "`;")) %>% as.numeric()

      fld_names <- dbGetQuery(db_conn, paste0("PRAGMA table_info(", tbl, ");")) %>%
        mutate(name_pk = paste0(name, if_else(pk == 1, accnt1("*"), ""))) %>%
        pull(name_pk) %>%
        paste(., collapse = "; ")

      indices_tbl <- dbGetQuery(db_conn, paste0("PRAGMA index_list(`", tbl, "`);"))
      if (nrow(indices_tbl) == 0) {
        indices_str <- "none"
      } else {
        indices_str <- indices_tbl %>% pull(name) %>% paste(., collapse = "; ")
      }

      res$tbls[[tbl_grp]][[tbl]] <- list(flds = fld_names, num_rows = num_rows, indices = indices_str)

    }

  }

  ## Disconnect if the user passed a filename. Otherwise, leave it open.
  if (x_type == "sqlite_fn") dbDisconnect(db_conn)

  ## Return
  class(res) <- c("ca_db_info", "list")
  invisible(res)

}


#' Print a ca_db_info object
#'
#' @param x An object of class ca_db_info
#' @param ... Unused
#' @method print ca_db_info
#' @export

print.ca_db_info <- function(x, ...) {
  cat(format(x), "\n")
}

#' Format a ca_db_info object for printing at the console
#'
#' @param x An object of class ca_db_info
#' @param ... Unused
#' @importFrom crayon green yellow bold cyan
#' @export
#' @method format ca_db_info

format.ca_db_info <- function(x, ...) {

  accent1 <- getOption("ca_accent1", paste0)
  accent2 <- getOption("ca_accent2", paste0)
  accent3 <- getOption("ca_accent3", paste0)
  accent4 <- getOption("ca_accent4", paste0)

  fn_str <- paste0(accent2("Source: "), x$fn, "\n")

  sql_str <- paste0(accent2("Get Everything SQL: "), "\"", x$sql, "\"\n")

  tbl_grp_headings <- list("val_tbl" = "Cal-Adapt data tables:",
                           "lkp_tbl" = "Lookup tables:",
                           "hash_tbl" = "API Request Hashes Tables:",
                           "other_tbl" = "Other tables:")

  tbls_str <- ""

  for (tbl_grp in names(tbl_grp_headings)) {

    if (length(x$tbls[[tbl_grp]]) > 0) {
      tbls_str <- paste0(tbls_str, "\n", accent2(tbl_grp_headings[[tbl_grp]]), "\n")

      for (tbl in names(x$tbls[[tbl_grp]])) {
        tbls_str <- paste0(tbls_str,
                           accent3(paste0(" ", tbl, "\n")),
                           paste0("   - ", accent4("Fields: "), paste(x$tbls[[tbl_grp]][[tbl]]$flds, collapse = ", "), "\n"),
                           paste0("   - ", accent4("Num rows: "), x$tbls[[tbl_grp]][[tbl]]$num_rows, "\n"),
                           paste0("   - ", accent4("Indices: "), x$tbls[[tbl_grp]][[tbl]]$indices, "\n"))
      }
    }
  }

  invisible(paste0(accent1("Cal-Adapt Query Database\n"),
                   fn_str, tbls_str, "\n", sql_str))

}


