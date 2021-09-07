#' View properties of a Cal-Adapt SQLlite database
#'
#' @param x Either a Cal-Adapt values remote tibble or a SQLite database file name
#'
#' @details
#' \code{x} can be either a remote tibble returned by \code{\link{ca_getvals_db}}, or a SQLite
#' database file name.
#'
#' @return
#' A list object with info about \code{x}, including the tables, fields, indices, and sql statements.
#'
#' @seealso \code{\link{ca_getvals_db}}
#'
#' @importFrom DBI dbConnect dbDisconnect dbListTables dbGetQuery dbListFields
#' @importFrom RSQLite SQLite
#' @importFrom dbplyr sql_render
#' @importFrom dplyr if_else mutate pull
#' @export

ca_db_info <- function(x) {

  ## Create a blank list to hold the results
  res <- list()
  res$sql <- list()

  if (is.character(x)) {     ## We have a sqlite fn
    x_type <- "sqlite_fn"
    if (length(x) != 1) stop("Only one file name can be passed to this function")
    if (!file.exists(x)) stop(paste0(x, " not found"))
    db_conn = dbConnect(SQLite(), dbname = x)
    res$fn <- x

  } else if (inherits(x, "ca_db")) {    ## We have a remote tibble
    x_type <- "ca_db"
    db_conn = x$src$con
    res$sql[[attr(x, "vals_tbl")]] <- sql_render(x)  ## this could get overwritten by the sidecar file
    res$fn <- db_conn@dbname %>% gsub("\\\\", "/", .)

  } else {
    stop("x must be a remote tibble returned by ca_getvals_db(), or an SQLite database filename")

  }

  ## Start an object to record the names of values tables found in the sidecar file
  vals_tbls_sidecar <- character(0)

  ## Read sidecar file (formatted in YAML), if it exists
  sc_fn <- paste0(res$fn, ".txt")
  if (file.exists(sc_fn)) {
    value_tbl <- NULL
    fcon <- file(sc_fn, open = "r")

    ## Loop thru the lines
    while ( TRUE ) {
      one_line <- readLines(fcon, n = 1, warn = FALSE)

      ## If this is the last line, stop
      if (length(one_line) == 0 ) break

      ## Determine if this line is comment
      first_char <- trimws(substr(one_line, 1, 1))
      if (first_char != "#" && first_char != "/") {

        ## Look for a colon in the line
        colon_pos <- regexpr(":", one_line)
        if (colon_pos > 0) {

          ## Get the key and value
          ln_key <- trimws(substring(one_line, 1, colon_pos - 1)[1])
          ln_val <- gsub("\"", "'", trimws(substring(one_line, colon_pos + 1)[1]))

          if (ln_key == "val_tbl") {
            vals_tbls_sidecar <- c(vals_tbls_sidecar, ln_val)
            value_tbl <- ln_val           ## val_tbl is being used to name the list element of the
                                        ## next incidence of xxxx_sql.
          }

          if (!is.null(value_tbl)) {
            if (tolower(ln_key) == tolower(paste0(value_tbl, "_sql"))) {
              res$sql[[value_tbl]] <- ln_val
            }
          }

        }  ## if colon_pos > 0

      }   ## this line is not a comment

    }  ## while TRUE
    close(fcon)

  } ## if  sidecar file found

  ## Define a color formatting function
  accnt1 <- getOption("ca_accent1", I)

  ## Get the tables in this database connection
  tbls <- dbListTables(db_conn)

  ## Put expected table names into groups, which is how they'll be grouped in the output
  tbl_grps_lst <- list()

  ## Get the indices of hashes tables
  ## FIX this will fail if no hashes are recorded
  tbl_grps_lst$hash_tbl <- grep("_hashes$", tbls)

  ## Get the indices of values tables
  potential_vals_tbls <- unique(c(vals_tbls_sidecar,
                           gsub("_hashes$", "", tbls[tbl_grps_lst$hash_tbl])))
  tbl_grps_lst$val_tbl <- which(tbls %in% intersect(tbls, potential_vals_tbls))

  ## Get the indices of lookup tables
  potential_lkp_tbls <- c("cvars", "gcms", "scenarios", "periods", "spags", "slugs")
  tbl_grps_lst$lkp_tbl <- which(tbls %in% intersect(tbls, potential_lkp_tbls))

  ## Get the indices of all other tables
  tbl_grps_lst$other_tbl <- setdiff(1:length(tbls),
                            c(tbl_grps_lst$hash_tbl, tbl_grps_lst$val_tbl, tbl_grps_lst$lkp_tbl))

  ## Create a list for the table groups
  res$tbls <- list()

  for (tbl_grp_idx in c("val_tbl", "lkp_tbl", "hash_tbl", "other_tbl")) {

    ## Create an element for these tables
    res$tbls[[tbl_grp_idx]] <- list()

    for (tbl in tbls[tbl_grps_lst[[ tbl_grp_idx]]]) {

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

      res$tbls[[tbl_grp_idx]][[tbl]] <- list(flds = fld_names, num_rows = num_rows, indices = indices_str)

    }

  }

  ## Disconnect if the user passed a filename. Otherwise, leave it open.
  if (x_type == "sqlite_fn") dbDisconnect(db_conn)

  ## Return
  class(res) <- c("ca_db_info", "list")

  res


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

  accent1 <- getOption("ca_accent1", I)
  accent2 <- getOption("ca_accent2", I)
  accent3 <- getOption("ca_accent3", I)
  accent4 <- getOption("ca_accent4", I)

  fn_str <- paste0(accent2("Source: "), x$fn, "\n")

  tbl_grp_headings <- list("val_tbl" = "Cal-Adapt data tables:",
                           "lkp_tbl" = "Lookup tables:",
                           "hash_tbl" = "API Request Hashes Tables:",
                           "other_tbl" = "Other tables:")

  tbls_str <- ""

  for (tbl_grp in names(tbl_grp_headings)) {

    if (length(x$tbls[[tbl_grp]]) > 0) {
      tbls_str <- paste0(tbls_str, accent2(tbl_grp_headings[[tbl_grp]]), "\n")

      for (tbl in names(x$tbls[[tbl_grp]])) {
        tbls_str <- paste0(tbls_str,
                           accent3(paste0(" ", tbl, "\n")),
                           paste0("   - ", accent4("Fields: "), paste(x$tbls[[tbl_grp]][[tbl]]$flds, collapse = ", "), "\n"),
                           paste0("   - ", accent4("Num rows: "), x$tbls[[tbl_grp]][[tbl]]$num_rows, "\n"),
                           paste0("   - ", accent4("Indices: "), x$tbls[[tbl_grp]][[tbl]]$indices, "\n"))
        if (tbl_grp == "val_tbl") {
          if (tbl %in% names(x$sql)) {
            tbls_str <- paste0(tbls_str,
                               paste0("   - ", accent4("SQL: "), x$sql[[tbl]], "\n"))
          }
        }
        tbls_str <- paste0(tbls_str, "\n")
      }
    }
  }

  invisible(paste0(accent1("Cal-Adapt Query Database\n"),
                   fn_str, "\n", tbls_str))
}


