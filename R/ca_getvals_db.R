#' Write values from an API request to a local database
#'
#' @param x A Cal-Adapt API request
#' @param db_fn File name of a SQLite database. See Details.
#' @param db_tbl The name of a database table. See Details.
#' @param omit_col Columns to exclude from the tibble
#' @param trans_len Number of APIs calls per write transaction. See Details.
#' @param indices Name of fields to index. See Details.
#' @param new_recs_only Write new records only to the database. See Details.
#' @param lookup_tbls Use lookup tables
#' @param lookup_ret_joined Return a table with lookup table fields, ignored if lookup_tbls = FALSE. See Details.
#' @param pause_n Number of API calls after which a built-in pause is triggered. See Details.
#' @param pause_secs Number of seconds to pause. See Details.
#' @param write_sidecar Save table metadata in a separate file. See Details.
#' @param stop_on_err Stop if the server returns an error
#' @param quiet Suppress messages
#' @param debug Print additional output at the console
#'
#' @details
#'
#' \code{ca_getvals_db} fetches data from the Cal-Adapt API and writes the data to a SQLite database as they're received. This
#' allows you to fetch relatively large volumes of data in the background, and potentially over multiple sessions as it will
#' pick up where it left off if interrupted. Saving the values in a database also reduces the risk of exhausting your RAM.
#'
#' Use \code{ca_getvals_db} to fetch large volumes of data (i.e., hundreds of thousands of values), or whenever you'd like
#' to keep  a local copy of the data. Note however for small amounts of data there is no advantage to putting it in a database
#' as it will be slighly slower to retrieve and work with.
#'
#' \code{db_fn} should be a file name with path to a SQLite database. A SQLIte database is a single file typically
#' with a \emph{.db} or \emph{.sqlite} extension. If the database doesn't exist, it will be created. If it already
#' exists, the new data will be added to it.
#'
#' \code{db_tbl} should the name of a table within the database where the new data will be saved. The table name should not
#' contain special characters and spaces are discouraged. If \code{new_recs_only = TRUE}, only new records will be
#' added to the database.
#'
#' \code{trans_len} defines the number of API calls per
#' \href{https://www.sqlitetutorial.net/sqlite-transaction/}{SQLite transaction} (i.e. how many API calls of data to
#' accumulate before doing a write operation to the database). This can speed things up. Set it to 0 to disable transactions.
#'
#' If \code{lookup_tbls = TRUE}, the database will create lookup tables for categorical columns such as GCM, scenario,
#' cvar, period, slug, etc. This can dramatically reduce the size of the SQLite database file and is generally recommended.
#' id \code{lookup_ret_joined = TRUE}, the tibble returned will have the lookup tables joined (i.e., column names will
#' be unaltered); if not the returned tibble will have id values for certain values. A small text file is created for each SQLite
#' database containing the names of the tables and SQL statement to join them (read automatically
#' by \code{\link{ca_db_read}}).
#'
#' \code{indices} is a vector of column names in \code{db_tbl} that you'd like indexed (ignored if \code{lookup_tbls = FALSE}).
#' Creating indices can improve the performance of filters and joins when you generate summaries, but at the cost of a larger
#' database file and slightly slower write operations. Fields you can create indices on include \code{"feat_id"} (the location id value),
#' \code{"cvar"}, \code{"gcm"}, \code{"scenario"}, \code{"period"}, \code{"slug"}, and \code{"spag"}.
#'
#' Indices can also be added to a SQLite database after downloading is complete with \link{ca_db_indices}. For large queries (e.g. thousands of
#' API calls), it is recommended to not build indices during the download process, and only add indices for those fields you
#' plan to filter on or join during your analysis. You can view which indices exist with \code{\link{ca_db_info}}.
#'
#' \code{pause_n} is the number of API calls after which a built-in pause of length \code{pause_secs} is triggered. This is intended
#' to avoid disruption on the Cal-Adapt server. The maximum value for \code{pause_n} is 2500, and the minimum value
#' for \code{pause_secs} is 30 seconds.
#'
#' The returned tibble is linked to the SQLite datbase. For the most part you can use the same dplyr functions to manipulate
#' the results, but to retrieve the actual values you need to use `collect()`. For more info working with a linked database,
#' see \url{https://dbplyr.tidyverse.org/articles/dbplyr.html}.
#'
#' @return A remote tibble linked to the SQLite database.
#'
#' @seealso \code{\link{ca_db_info}}, \code{\link{ca_db_indices}}, \code{\link{ca_db_read}},
#'
#' @import crayon
#' @importFrom httr GET content modify_url accept_json http_error stop_for_status warn_for_status http_status
#' @importFrom utils txtProgressBar setTxtProgressBar packageVersion
#' @importFrom dplyr select mutate left_join tbl sql bind_rows
#' @importFrom curl has_internet
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable dbListTables dbCreateTable dbReadTable dbExecute dbIsValid dbListFields
#' @importFrom RSQLite SQLite dbBegin dbCommit
#' @importFrom fastmatch `%fin%`
#' @export

ca_getvals_db <- function(x, db_fn, db_tbl, omit_col = NULL, indices = NULL, new_recs_only = TRUE,
                          trans_len = 100, lookup_tbls = TRUE, lookup_ret_joined = TRUE,
                          pause_n = 1000, pause_secs = 60,
                          write_sidecar = TRUE, stop_on_err = TRUE, quiet = FALSE, debug = FALSE) {

  if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq", call. = FALSE)

  ## Check for an internet connection
  if (!has_internet()) stop("No internet connection detected", call. = FALSE)

  ## Get the functions to format messages
  accent2 <- getOption("ca_accent2", I)
  msg_fmt <- getOption("ca_message", I)

  ## Check the table name for spaces
  if (grepl(" ", db_tbl)) stop("Please provide a table name that doesn't have spaces", call. = FALSE)

  if (pause_n > 2500) stop("The maximum value for `pause_n` is 2500", call. = FALSE)
  if (pause_secs < 30) stop("The smallest value for `pause_secs` is 30", call. = FALSE)

  ## Get a tibble with the individual API calls
  apicalls_lst <- ca_apicalls(x, check_for = "getvals")
  api_tbl <- apicalls_lst$api_tbl

  ## Get the name of first column
  feat_id_fldname <- apicalls_lst$idfld

  ## Get the cache dir to save temporary geojson files
  ## (we save them here rather than tempdir so httptest will
  ## generate the same hash for the API call each time)
  ca_cache_dir <- ca_getcache()

  ## Rename the first column (feature id)
  names(api_tbl)[1] <- feat_id_fldname

  ## Make sure all the slugs are returning the same units
  if (length(unique(api_tbl$rs_units)) > 1) {
    stop("Can not process this API request: Raster series have different units", call. = FALSE)
  }

  ## Verify indices contains valid values
  if (!is.null(indices) > 0) {
    valid_index_names <- c(feat_id_fldname, "feat_id", "cvar", "gcm", "scenario", "period", "spag", "slug")

    if (FALSE %in% (indices %in% valid_index_names)) {
      stop(paste0("Valid values for indices include: ", paste(valid_index_names, collapse = ","), ","), call. = FALSE)
    }
  }

  #########################################################################################
  ## Define which columns to save in the output table
  if (!identical(x$slug, NA)) {
    apicall_cols_default <- c(feat_id_fldname, "slug", "spag")
  } else if (identical(x$livneh, TRUE)) {
    apicall_cols_default <- c(feat_id_fldname, "cvar", "period", "slug", "spag")
  } else {
    apicall_cols_default <- c(feat_id_fldname, "cvar", "scenario", "gcm", "period", "spag")
  }

  ## Remove any columns the user wants to omit
  if (!is.null(omit_col)) {
    if ("val" %in% omit_col) stop("Sorry, you can not omit the values column from the results", call. = FALSE)
    if ("dt" %in% omit_col) stop("Sorry, you can not omit the date column from the results", call. = FALSE)
    if (feat_id_fldname %in% omit_col) {
      stop("Sorry, you can not omit the location column from the results", call. = FALSE)
    }
  }
  apicall_cols_keep <- setdiff(apicall_cols_default, omit_col)
  ###########################################################################################

  ## Next we need to
  ## 1) Create the tibbles for the lookup tables
  ## 2) join them to api_tbl,
  ## 3) Define the columns to save in the output table, and
  ## 4) create a SQL string in case lookup_ret_joined = TRUE

  if (lookup_tbls) {

    ## Create lookup tibbles so we can join them to api_tbl (as needed)
    ## Even if we don't need them all, they're small and easy to generate

    cvar_tbl <- tibble(cvar_id = 1:length(cvars), cvar = cvars)
    period_tbl <- tibble(period_id = 1:length(periods), period = periods)
    gcm_tbl <- tibble(gcm_id = 1:length(gcms), gcm = gcms)
    scenario_tbl <- tibble(scenario_id = 1:length(scenarios), scenario = scenarios)
    spag_vals <- c("none", "mean", "max", "median", "min", "sum")
    spag_tbl <- tibble(spag_id = 1:length(spag_vals), spag = spag_vals)

    ## Left join lookup tables to api_tbl (all except slug)
    stnd_lkp_tbls <- c("cvar", "period", "gcm", "scenario", "spag")
    if (TRUE %in% (apicall_cols_keep %in% stnd_lkp_tbls)) {
      left_joins_cmd <- "api_tbl <- api_tbl"
      for (my_col in stnd_lkp_tbls) {     ## not doing slug right here, not sure why
        if (my_col %in% apicall_cols_keep) {
          left_joins_cmd <- paste0(left_joins_cmd,
                                   paste0(" %>% left_join(", my_col, "_tbl, by=\"", my_col, "\")"))
        }
      }
      eval(parse(text = left_joins_cmd))
    }

    ## Construct the SQL for this table
    if (length(apicall_cols_keep) == 1) {
      tbl_sql <- paste0("SELECT ", feat_id_fldname, ", dt, val FROM ", db_tbl)
    } else {
      tbl_sql <- paste0("SELECT ", feat_id_fldname, ", ",
                        paste(apicall_cols_keep[2:length(apicall_cols_keep)], collapse = ", "),
                        ", dt, val FROM ", db_tbl)

      for (lkp_col in apicall_cols_keep[2:length(apicall_cols_keep)]) {
        tbl_sql <- paste0(tbl_sql, " LEFT JOIN ", lkp_col, "s ON ", db_tbl,
                          ".", lkp_col, "_id = ", lkp_col, "s.", lkp_col, "_id")
      }
    }

    ## Add '_id' to all elements of apicall_cols_keep after #1
    if (length(apicall_cols_keep) > 1) {
      apicall_cols_keep[2:length(apicall_cols_keep)] <- paste0(apicall_cols_keep[2:length(apicall_cols_keep)], "_id")
    }

  } else {
    ## apicall_cols_keep - already defined, no changes needed
    tbl_sql <- NA   ## No lookup tables
  }

  ## Close any open connections leftover from previous function calls that were interrupted.
  ## NOT SURE IF THIS IS NEEDED, GETTING INCONSISTENT RESULTS
  ## IF I DON'T KEEP THIS REMOVE dbIsValid FROM IMPORTS
  if (!is.null(getOption("ca_dbconn"))) {

    if (dbIsValid(getOption("ca_dbconn"))) {
      if (debug) message(msg_fmt(" - found a leftover open database connection, will try to close it"))
      dbDisconnect(getOption("ca_dbconn"))
    }

    options("ca_dbconn" = NULL)
  }

  #####################################################################
  ## CREATE THE SQLITE DATABASE AND VARIOUS TABLES IF NEEDED

  ## Create a connection and enable referential integrity (for this connection only, its not persistent)
  mydb <- dbConnect(SQLite(), db_fn)

  ## Set up auto-disconnect - NEW THINKING - DONT WANT TO DO THIS
  #on.exit(dbDisconnect(mydb))

  ## Record this connection in an option in case there is an unexpected termination
  options("ca_dbconn" = mydb)

  ## on.exit(options("ca_dbconn" = NULL))

  ## Enforce referential integrity (this connection only)
  dbExecute(conn = mydb, "PRAGMA foreign_keys = ON")

  if (debug) message(msg_fmt(paste0(" - new values will be saved to `", db_tbl, "` in: ", db_fn)))

  ## Get all tables currently in the database
  all_tbls <- dbListTables(mydb)

  ## Construct the name of the table that saves processed api_urls
  db_hashes_tbl <- paste0(db_tbl, "_hashes")

  ## If the hash_int table exists, get the list of saved hashes
  if (db_hashes_tbl %in% all_tbls) {
    hashes_already_in_db <- dbReadTable(mydb, db_hashes_tbl)[["hash_int"]]
    if (debug) message(msg_fmt(paste0(" - found ", length(hashes_already_in_db), " API calls recorded in ", db_hashes_tbl)))

  } else {
    ## If not, create the table now and initialize hashes_already_in_db
    dbExecute(mydb, paste0("CREATE TABLE `",
                           db_hashes_tbl,
                           "` (hash_int INTEGER PRIMARY KEY);"))
    hashes_already_in_db <- integer(0)
    if (debug) message(msg_fmt(paste0(" - created table: ", db_hashes_tbl)))
  }

  ## If using lookup tables, create those now
  if (lookup_tbls) {

    ## Get a like of lookup tables
    lkp_tbl_names <- apicall_cols_keep[-1] %>% gsub("_id$", "", .)

    ## If needed, create the lookup tables for cvar, period, gcm, scenario, and spag.
    ## (but not slug, which is treated separately1)
    for (lkp in lkp_tbl_names[lkp_tbl_names != "slug"]) {
      if (!paste0(lkp, "s") %in% all_tbls) {

        ## Create the table defining the primary key
        dbExecute(mydb,
                  paste0("CREATE TABLE ", lkp, "s ",
                         "( ", lkp, "_id INTEGER PRIMARY KEY, ",
                         lkp, " TEXT NOT NULL);")
        )

        ## Next, add rows from the corresponding tibble
        dbWriteTable(mydb,
                     name = paste0(lkp, "s"),
                     value = get(paste0(lkp, "_tbl")),
                     append = TRUE)

        if (debug) message(msg_fmt(paste0(" - created lookup table: ", lkp, "s")))

      }
    }

    ## Deal with slug lookup table
    if ("slug" %in% lkp_tbl_names) {

      ## Create the slugs table if its not already there
      if (!"slugs" %in% all_tbls) {
        ## Create the table defining the primary key
        dbExecute(mydb,
                  "CREATE TABLE slugs ( slug_id INTEGER PRIMARY KEY, slug TEXT NOT NULL);")
        if (debug) message(msg_fmt(" - created lookup table: slugs"))
      }

      ## Read rows from the slugs tibble in the database (which could be brand new, or could have
      ## slugs from previous calls)
      saved_slugs_df <- dbReadTable(mydb, "slugs")

      ## If this API request includes some new slugs, write them to the database
      new_slugs <- setdiff(unique(api_tbl$slug), saved_slugs_df$slug)
      if (length(new_slugs) > 0) {
        new_slugs_tbl <- tibble(slug_id = (1:length(new_slugs)) + nrow(saved_slugs_df),
                                slug = new_slugs)
        dbWriteTable(mydb,
                     name = "slugs",
                     value = new_slugs_tbl,
                     append = TRUE)

        saved_slugs_df <- saved_slugs_df %>% bind_rows(new_slugs_tbl)

      }

      ## Add the slug id colunn to api_tbl
      api_tbl <- api_tbl %>% left_join(saved_slugs_df, by = "slug")

    }

  }
  ## Done creating the sqlite database and all lookup tables
  #####################################################################


  ## If the values table already exists, cross-check that it has all the required fields.
  ## (If not, it could have been created with a different value for lookup_tbls)
  if (db_tbl %in% all_tbls) {
    if (FALSE %in% (apicall_cols_keep %in% dbListFields(mydb, db_tbl))) {
      stop(paste0(db_tbl, " already exists, but doesn't have the required fields. Specify a different database or a different values table."), call. = FALSE)
    }
  }

  ## Start the first database write transaction
  if (trans_len > 0) {
    dbBegin(mydb)
    calls_made <- 0
  }

  ## Define the user agent
  caladaptr_ua <- user_agent(paste0("caladaptr_v", packageVersion("caladaptr")))

  ## Create a tibble to store the results
  res_tbl <- NULL

  ## Determine if this set of API calls use sf objects
  aoi_sf <- inherits(apicalls_lst$loc_sf, "sf")

  ## Compute the file names which will be used to export individual features to temporary geojson files
  ## There should be one file name for each feature (row)
  if (aoi_sf) {

    features_hashes <- unname(sapply(apicalls_lst$loc_sf %>%
                                       st_geometry() %>% st_as_text(),
                                     digest,
                                     algo = "crc32",
                                     file = FALSE))

    gjsn_all_fn <- file.path(ca_cache_dir,
                             paste0("~ca_", features_hashes, ".geojson"))

    if (anyDuplicated(gjsn_all_fn) > 0) warning(red("Duplicate feature hashes(s) encountered"))

  }

  ## If a progress bar is needed, set it up
  use_pb <- !quiet && !debug && (nrow(api_tbl) > 3)
  if (use_pb) pb <- txtProgressBar(min = 1, max = nrow(api_tbl), style = 3)

  ## Loop through calls
  if (debug) message(msg_fmt(paste0(" - going to make ", nrow(api_tbl), " api calls")))

  # num_vals_downloaded <- 0   ## not using this any more, pauses based on the number of API calls

  ## Create a counter for the number of API calls made (used to determine when we need to pause)
  calls_made <- 0

  for (i in 1:nrow(api_tbl)) {

    if (use_pb) {setTxtProgressBar(pb, i)}

    ## First order of business - see if this API call has already been made
    hash_int <- api_tbl[i, "hash_int", drop = TRUE]
    hash_in_db <- hash_int %fin% hashes_already_in_db

    if (new_recs_only) {
      make_the_call <- !hash_in_db
      if (debug && !make_the_call) message(msg_fmt(paste0(" - api call already run, skipping")))
    } else {
      make_the_call <- TRUE
    }

    ## Next, call the API
    if (make_the_call) {

      ## Check if we need to do a pause
      calls_made <- calls_made + 1
      if (calls_made %% pause_n == 0) {
        if (!quiet) message(accent2(paste0(" - pausing for ", pause_secs, " seconds to let the server catch its breath!")))
        Sys.sleep(pause_secs)
      }

      if (aoi_sf) {

        ## Get the temporary geojson file name for this row (which is stored in the loc_qry column)
        gjsn_fn <- gjsn_all_fn[api_tbl[i, "loc_qry", drop = TRUE]]


        if (!file.exists(gjsn_fn)) {
          if (debug) message(msg_fmt(paste0(" - saving temp geojson: ", basename(gjsn_fn))))
          st_write(apicalls_lst$loc_sf %>%
                     slice(api_tbl[i, "loc_qry", drop = TRUE]),
                   dsn = gjsn_fn,
                   quiet = TRUE)
        }

        ## Create values for parameters (setting them to NULL if not used)
        if (is.na(api_tbl[i, "start", drop = TRUE])) {
          start_dt <- NULL
        } else {
          start_dt <- as.character(api_tbl[i, "start", drop = TRUE])
        }

        if (is.na(api_tbl[i, "end", drop = TRUE])) {
          end_dt <- NULL
        } else {
          end_dt <- as.character(api_tbl[i, "end", drop = TRUE])
        }

        if (is.na(api_tbl[i, "spag", drop = TRUE]) || api_tbl[i, "spag", drop = TRUE] == "none") {
          spatial_ag <- NULL
        } else {
          spatial_ag <- as.character(api_tbl[i, "spag", drop = TRUE])
        }

        body_flds_lst <- list(features = upload_file(gjsn_fn),
                              start = start_dt,
                              end = end_dt,
                              stat = spatial_ag)

                              # format = "json" removed in favor of accept_json()

        post_url <- api_tbl[i, "api_url", drop = TRUE]

        if (debug) {
          feat_id <- api_tbl[i, 1, drop = TRUE]
          message(msg_fmt(paste0(" - (", i, "/", nrow(api_tbl), ") PUT ", post_url, ". (", feat_id_fldname, "=",
                                feat_id, ", start='", start_dt, "', end='", end_dt, "', stat='", spatial_ag, "')")))
        }

        ## Make the POST request
        qry_resp <- POST(url = post_url,
                         body = body_flds_lst,
                         encode = "multipart",
                         accept_json(),
                         caladaptr_ua)

                        # content_type_json(), - causes a 400 error

      } else {   ## SEND A GET REQUEST

        api_url_full <- api_tbl[i, "api_url", drop = TRUE]

        if (debug) message(msg_fmt(paste0(" - (", i, "/", nrow(api_tbl), ") ", api_url_full)))

        ## Make request
        qry_resp <- GET(api_url_full, accept_json(), caladaptr_ua)

      }

      ######################################################################
      ## AT THIS POINT, WE HAVE RESPONSE

      ## See if the server sent an error
      if (http_error(qry_resp)) {

        if (stop_on_err) {
          stop_for_status(qry_resp)

        } else {

          ## Don't stop - print a message or generate a warning
          if (quiet && !debug) {
            warn_for_status(qry_resp)
          } else {
            message(red(paste0(" - Oh dear. ", http_status(qry_resp)$message)))
          }

        }


      } else {

        ## Call was successful - next convert response content to a list
        qry_content <- content(qry_resp, type = "application/json")

        if (length(qry_content) > 0) {

          ## Get the data values (one per date)
          these_vals <- unlist(qry_content$data)

          ## Not converting to units object because that isn't compatible with SQLite

          ## Time to write out some data!! First see if the 'values' table has been created or not.
          new_tbl <- !db_tbl %in% dbListTables(mydb)

          if (new_tbl) {

            ## We have to create the values table. It doesn't exist yet.

            ## Get the field type for the feat_id column
            if (is.integer(api_tbl[[feat_id_fldname]])) {
              feat_id_type <- "INTEGER"
            } else if (is.numeric(api_tbl[[feat_id_fldname]])) {
              feat_id_type <- "NUMERIC"
            } else if (is.character(api_tbl[[feat_id_fldname]])) {
              feat_id_type <- "TEXT"
            } else {
              stop("DONT KNOW THE FIELD TYPE OF THE FEAT_ID COLUMN IN API_TBL. PLEASE CONTACT THE PACKAGE AUTHOR FOR HELP.")
            }

            if (lookup_tbls) {

              # sql_flds and sql_indices are both pieces of SQL expression that we will build up.
              ## They will ultimately be used to create the table and indices

              ## Start the list of field definitions with the feat_id column
              sql_flds <- paste0(feat_id_fldname, " ", feat_id_type, " NOT NULL")

              ## Initialize a variable to hold a SQL expression that will be used to create the indices
              sql_indices <- character(0)

              ## Add an index expression for the feature id
              if ("feat_id" %in% indices || feat_id_fldname %in% indices) {
                sql_indices <- c(sql_indices,
                                 paste0("CREATE INDEX `", db_tbl, ".", feat_id_fldname,
                                        "` ON `", db_tbl, "`(`", feat_id_fldname, "`);"))
              }

              ## Define the lookup tables that are needed
              flds_make <- lkp_tbl_names

              ## Add field definition and index creation expressions for other fields
              for (fld in flds_make) {
                sql_flds <- c(sql_flds,
                              paste0(fld, "_id INTEGER NOT NULL REFERENCES ", fld, "s"))

                if (fld %in% indices) {
                  sql_indices <- c(sql_indices,
                                   paste0("CREATE INDEX `", db_tbl, ".", fld, "_id` ON `", db_tbl, "`(`", fld, "_id`);"))
                }
              }


              ## Create the values table

              ## Add additional field expressions  for the values table for dt and val (no indices needed)
              sql_flds <- c(sql_flds, "dt TEXT NOT NULL", "val NUMERIC NOT NULL")   ## val could also be REAL?

              ## Create the values table
              sql_create_my_table <- paste0("CREATE TABLE `", db_tbl, "` (",
                                            paste(sql_flds, collapse = ", "), ");")
              dbExecute(mydb, sql_create_my_table)

              # Generate indices for the foreign key fields in the vals table
              for (sql_create_index in sql_indices) {
                dbExecute(mydb, sql_create_index)
              }

            }

          }

          ## Write the data. This statement works regardless of using lookup tables or not
          dbWriteTable(mydb, name = db_tbl,
                       value = data.frame(api_tbl[i, apicall_cols_keep],
                                          dt = substr(unlist(qry_content$index), 1, 10),
                                          val = these_vals),
                       append = TRUE)

          ## Next, write the hash_int value to the hashes table if it wasn't found
          ## earlier (regardless of new_recs_only).
          ## No need to append it to hashes_already_in_db because this call wont be called
          ## again in this loop

          if (!hash_in_db) {
            dbWriteTable(mydb, name = db_hashes_tbl,
                         value = data.frame(hash_int = hash_int),
                         append = TRUE)
          }

          ## Commit the transaction every nth call
          if (trans_len > 0) {
            calls_made <- calls_made + 1
            if (calls_made %% trans_len == 0) {
              ##dbExecute(mydb, "COMMIT;")
              dbCommit(mydb)
              if (debug) message(accent2(" - COMMIT"))

              ## num_vals_downloaded, " values downloaded since last pause point.")))

              ## Start a new transaction
              dbBegin(mydb)

            }
          }

        } else {
          ## Nothing returned - date fell outside of range? location outside extent?
          if (debug) message(msg_fmt(" - no values returned!"))
        }

      }

    }   ## if make_the_call

  }  ## done with the loop  for (i in 1:nrow(api_tbl))

  ## Close the progress bar
  if (use_pb) close(pb)

  ## Commit the final write
  if (trans_len > 0 && calls_made > 0) {
    dbCommit(mydb)
    if (debug) message(accent2(" - FINAL COMMIT"))
  }

  ## Don't close the DBI connection, need to leave it open so the tibble returned will work

  ## Delete any temporary geojson files created
  if (aoi_sf) {
    tmp_jsn_fn <- list.files(ca_cache_dir, pattern = "^\\~ca_(.*).geojson$", full.names = TRUE)
    if (length(tmp_jsn_fn) > 0) {
      if (debug) message(msg_fmt(paste0(" - deleting ", length(tmp_jsn_fn), " temp geojson files")))
      unlink(tmp_jsn_fn)
    }
  }

  ## Return a remote tibble
  if (is.na(tbl_sql) || !lookup_ret_joined) {
    res <- tbl(mydb, db_tbl)
  } else {
    if (debug) message(msg_fmt(paste0(" - returning a remote tbl based on SQL: \"", tbl_sql, "\"")))
    res <- tbl(mydb, sql(tbl_sql))
  }

  if (write_sidecar) {
    val_tbl_info <- paste0("val_tbl: ", db_tbl, "\n",
                      db_tbl, "_sql: ", tbl_sql, "\n\n")

    sidecar_fn <- paste0(db_fn, ".txt") %>% gsub("\\\\", "/", .)

    if (file.exists(sidecar_fn)) {

      ## The sidecar file already exists. We need to scan it and see if there's a line for this values table:
      ## example val_tbl: sac_pts_tbl
      ## If yes - do nothing
      ## If no - append it

      tbl_found <- FALSE
      fcon <- file(sidecar_fn, open = "r")

      ## Loop thru the lines of the sidecar file
      while (TRUE) {
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

            if (ln_key == "val_tbl" && ln_val == db_tbl) {
              tbl_found <- TRUE
              break
            }

          }
        }

      }  ## while TRUE
      close(fcon)

      if (!tbl_found) {
        cat(val_tbl_info, file = sidecar_fn, append = TRUE)
        # message("JUST APPENDED A NEW TABLE INFO")
      } else {
        # message("TABLE INFO IS ALREADY IN THE SIDECAR, SKIPPPING")
      }

    } else {
      cat("# Table metadata for: ", db_fn, "\n\n",
          val_tbl_info,
          file = sidecar_fn,
          sep = "")

    }

  }

  class(res) <- c(class(res), "ca_db")
  attr(res, "lkp_sql") <- tbl_sql
  attr(res, "vals_tbl") <- db_tbl
  invisible(res)

}


