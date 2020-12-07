#' Write values from an API request to a local database
#'
#' @param x A Cal-Adapt API request
#' @param db_fn File name of a SQLite database. See Details.
#' @param db_tbl The name of a database table. See Details.
#' @param trans_len Number of APIs calls per write transaction. See Details.
#' @param indices Name of fields to index. See Details.
#' @param new_recs_only Write new records only to the database. See Details.
#' @param lookup_tbls Use lookup tables
#' @param lookup_ret_joined Return a table with lookup table fields, ignored if lookup_tbls = FALSE. See Details.
#' @param pause_n Number of API calls after which a built-in pause is triggered. See Details.
#' @param pause_secs Number of seconds to pause. See Details.
#' @param stop_on_err Stop if the server returns an error
#' @param quiet Suppress messages
#' @param debug Print additional output at the console
#'
#' @details
#'
#' Use \code{ca_getvals_db} when retrieving large volumes of data (i.e., tens of thousands of values). This function
#' writes the data to a SQLite database as they're received so you don't run the risk of exhausting your RAM. Another
#' benefit is if the operation is interrupted before it finished (e.g., due to an internet hiccup), it will pick up
#' where it left off. Note however for small amounts of data there is no advantage to putting it in a database
#' as it will be slower to retrieve and work with.
#'
#' \code{db_fn} should be a file name with path to a SQLite database. A SQLIte database is a single file typically
#' with a \emph{.db} or \emph{.sqlite} extension. If the database doesn't exist, it will be created. If it already
#' exists, the new data will be added to it.
#'
#' \code{db_tbl} should the name of a table within the database where the new data will be saved. The table name should not
#' contain special characters and spaces are discouraged. If \code{new_recs_only = TRUE}, only new records will be
#' added to the database. \code{trans_len} defines the number of API calls per
#' \url{https://www.sqlitetutorial.net/sqlite-transaction/}{SQLite transaction} (i.e. how many API calls of data to
#' accumulate before doing a write operation to the database). This can speed things up. Set it to 0 to disable transactions.
#'
#' If \code{lookup_tbls = TRUE}, the database will create lookup tables for categorical columns such as GCM, scenario,
#' cvar, period, slug, etc. This can dramatically reduce the size of the SQLite database file and is generally recommended.
#' id \code{lookup_ret_joined = TRUE}, the tibble returned will have the lookup tables joined (i.e., column names will
#' be unaltered); if not the returned tibble will have id values for certain values.
#'
#' \code{indices} is a vector of column names that you'd like indexed (ignored if \code{lookup_tbls = FALSE}). Creating indices can
#' improve the performance of filters and joins, but at the cost of a larger database file and slower write operations. Fields you can
#' create indices on include \code{"feat_id"} (the location id value), \code{"cvar"}, \code{"gcm"}, \code{"scenario"}, \code{"period"},
#' \code{"slug"}, and \code{"spag"}.
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
#' @seealso \code{\link{ca_db_info}}, \code{\link{ca_db_indices}}
#'
#' @import crayon
#' @importFrom httr GET content modify_url accept_json http_error stop_for_status warn_for_status
#' @importFrom utils txtProgressBar setTxtProgressBar packageVersion
#' @importFrom dplyr select mutate left_join tbl sql bind_rows
#' @importFrom curl has_internet
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable dbListTables dbCreateTable dbReadTable dbExecute dbIsValid
#' @importFrom RSQLite SQLite dbBegin dbCommit
#' @importFrom fastmatch `%fin%`
#' @export

ca_getvals_db <- function(x, db_fn, db_tbl, indices = NULL, new_recs_only = TRUE,
                          trans_len = 100, lookup_tbls = TRUE, lookup_ret_joined = TRUE,
                          pause_n = 1000, pause_secs = 60,
                          stop_on_err = TRUE, quiet = FALSE, debug = TRUE) {

  if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq")

  ## Check for an internet connection
  if (!has_internet()) stop("No internet connection detected")

  accent2 <- getOption("ca_accent2", paste0)

  ## Check the table name for spaces
  if (grepl(" ", db_tbl)) stop("Please provide a table name that doesn't have spaces")

  if (pause_n > 2500) stop("The maximum value for `pause_n` is 2500")
  if (pause_secs < 30) stop("The smallest value for `pause_secs` is 30")

  ## Get a tibble with the individual API calls
  apicalls_lst <- ca_apicalls(x)
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
    stop("Can not process this API request: Raster series have different units")
  }

  ## Verify indices contains valid values
  if (!is.null(indices) > 0) {
    valid_index_names <- c(feat_id_fldname, "feat_id", "cvar", "gcm", "scenario", "period", "spag", "slug")

    if (FALSE %in% (indices %in% valid_index_names)) {
      stop(paste0("Valid values for indices include: ", paste(valid_index_names, collapse = ","), ","))
    }
  }

  ## API Request is based on gcm+scenario+cvar+period
  if (identical(x$slug, NA)) {

    if (lookup_tbls) {
      ## 1) Create the tibbles for the lookup tables, 2) join them to api_tbl,
      ## 3) Define the columns to save in the output table (either in-memory tibble or db), and
      ## 4) create a SQL string in case lookup_ret_joined = TRUE

      ## Create lookup tibbles and join them to api_tbl
      cvar_tbl <- tibble(cvar_id = 1:length(cvars), cvar = cvars)
      period_tbl <- tibble(period_id = 1:length(periods), period = periods)
      gcm_tbl <- tibble(gcm_id = 1:length(gcms), gcm = gcms)
      scenario_tbl <- tibble(scenario_id = 1:length(scenarios), scenario = scenarios)
      spag_vals <- c("none", "mean", "max", "median", "min", "sum")
      spag_tbl <- tibble(spag_id = 1:length(spag_vals), spag = spag_vals)

      api_tbl <- api_tbl %>%
        left_join(cvar_tbl, by = "cvar") %>%
        left_join(period_tbl, by = "period") %>%
        left_join(gcm_tbl, by = "gcm") %>%
        left_join(scenario_tbl, by = "scenario") %>%
        left_join(spag_tbl, by = "spag")

      apicall_cols_keep <- c(names(api_tbl)[1], "cvar_id", "period_id", "gcm_id", "scenario_id", "spag_id")

      tbl_sql <- paste0("SELECT ", feat_id_fldname, ", cvar, gcm, period, scenario, spag, dt, val FROM ",
                        db_tbl, " LEFT JOIN cvars ON ", db_tbl, ".cvar_id = cvars.cvar_id ",
                        "LEFT JOIN gcms ON ", db_tbl, ".gcm_id = gcms.gcm_id ",
                        "LEFT JOIN periods ON ", db_tbl, ".period_id = periods.period_id ",
                        "LEFT JOIN scenarios ON ", db_tbl, ".scenario_id = scenarios.scenario_id ",
                        "LEFT JOIN spags ON ", db_tbl, ".spag_id = spags.spag_id")

    } else {
      ## No lookup tables
      apicall_cols_keep <- c(names(api_tbl)[1], "cvar", "period", "gcm", "scenario", "spag")
      tbl_sql <- NA

    }


  } else {   ## API request is based on slug(s)

    if (lookup_tbls) {
      ## 1) Create the tibbles for the lookup tables, 2) join them to api_tbl,
      ## 3) Define the columns to save in the output table (either in-memory tibble or db), and
      ## 4) create a SQL string in case lookup_ret_joined = TRUE

      slug_all <- unique(api_tbl$slug)
      #slug_tbl <- tibble(slug_id = 1:length(slug_all), slug = slug_all)   change this - use slug_id from ca_catalog_rs()

      spag_vals <- c("none", "mean", "max", "median", "min", "sum")
      spag_tbl <- tibble(spag_id = 1:length(spag_vals), spag = spag_vals)

      #stop("RIGHT HERE I NEED TO UPDATE THIS SECTION FOR SLUG_IG AND SPAG_ID")
      api_tbl <- api_tbl %>% left_join(spag_tbl, by = "spag")   ## we'll join slug_id below

      apicall_cols_keep <- c(names(api_tbl)[1], "slug_id", "spag_id")

      tbl_sql <- paste0("SELECT ", feat_id_fldname, ", slug, spag, dt, val FROM ",
                        db_tbl, " LEFT JOIN slugs ON ", db_tbl, ".slug_id = slugs.slug_id ",
                        "LEFT JOIN spags ON ", db_tbl, ".spag_id = spags.spag_id")


    } else {
      # Not using lookup tables
      apicall_cols_keep <- c(names(api_tbl)[1], "slug", "spag")
      tbl_sql <- NA

    }
  }

  ## Close any open connections leftover from previous function calls that were interrupted.
  ## NOT SURE IF THIS IS NEEDED, GETTING INCONSISTENT RESULTS
  ## IF I DON'T KEEP THIS REMOVE dbIsValid FROM IMPORTS
  if (!is.null(getOption("ca_dbconn"))) {

    if (dbIsValid(getOption("ca_dbconn"))) {
      if (debug) message(silver(" - found a leftover open database connection, will try to close it"))
      dbDisconnect(getOption("ca_dbconn"))
    }

    # else {
    # if (debug) message(yellow(" - Found a leftover connection but it isn't valid, ignoring"))

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

  if (debug) message(silver(paste0(" - new values will be saved to `", db_tbl, "` in: ", db_fn)))

  ## Get all tables currently in the database
  all_tbls <- dbListTables(mydb)

  ## Construct the name of the table that saves processed api_urls
  db_hashes_tbl <- paste0(db_tbl, "_hashes")

  ## If the hash_int table exists, get the list of saved hashes
  if (db_hashes_tbl %in% all_tbls) {
    hashes_already_in_db <- dbReadTable(mydb, db_hashes_tbl)[["hash_int"]]
    if (debug) message(silver(paste0(" - found ", length(hashes_already_in_db), " API calls recorded in ", db_hashes_tbl)))

  } else {
    ## If not, create the table now and initialize hashes_already_in_db
    dbExecute(mydb, paste0("CREATE TABLE `",
                           db_hashes_tbl,
                           "` (hash_int INTEGER PRIMARY KEY);"))
    hashes_already_in_db <- integer(0)
    if (debug) message(silver(paste0(" - created table: ", db_hashes_tbl)))
  }

  ## If using lookup tables, create those now
  if (lookup_tbls) {

    if (identical(x$slug, NA)) {

      ## Create the lookup tables for cvar, period, gcm, scenario, and spag
      for (lkp in c("cvar", "period", "gcm", "scenario", "spag")) {

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

          if (debug) message(silver(paste0(" - created lookup table: ", lkp, "s")))

        }

      }

      ## At this point all the lookup tables should be in the database!!
      # browser()
      # dbListTables(mydb)
      # message(("At this point all the lookup tables should be in the database!!"))
      # res <- dbSendQuery(mydb, "PRAGMA foreign_keys")
      # message((paste0("foreign_keys = ", dbFetch(res)[1,1])))
      # dbClearResult(res)


    } else {
      ## Create the lookup tables for slug and spag

      if (!"spags" %in% all_tbls) {
        ## Create the table defining the primary key
        dbExecute(mydb, "CREATE TABLE spags ( spag_id INTEGER PRIMARY KEY, spag TEXT NOT NULL);")

        ## Next, add rows from the corresponding tibble
        dbWriteTable(mydb, name = "spags", value = spag_tbl, append = TRUE)

        if (debug) message(silver(" - created lookup table: spags"))
      }

      if (!"slugs" %in% all_tbls) {
        ## Create the table defining the primary key
        dbExecute(mydb,
                  "CREATE TABLE slugs ( slug_id INTEGER PRIMARY KEY, slug TEXT NOT NULL);")
        if (debug) message(silver(" - created lookup table: slugs"))
      }

      ## Next, add rows from the corresponding tibble
      saved_slugs <- dbReadTable(mydb, "slugs")

      ## If there are some new slugs, write them to the database
      new_slugs <- setdiff(slug_all, saved_slugs$slug)
      if (length(new_slugs) > 0) {
        new_slugs_tbl <- tibble(slug_id = (1:length(new_slugs)) + nrow(saved_slugs),
                                slug = new_slugs)
        dbWriteTable(mydb,
                     name = "slugs",
                     value = new_slugs_tbl,
                     append = TRUE)

        saved_slugs <- saved_slugs %>% bind_rows(new_slugs_tbl)

      }

      ## Add the slug id colunn
      api_tbl <- api_tbl %>% left_join(saved_slugs, by = "slug")

    }

  }
  ## Done creating the sqlite database and all lookup tables
  #####################################################################

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
  if (debug) message(silver(paste0(" - going to make ", nrow(api_tbl), " api calls")))

  # num_vals_downloaded <- 0   ## not using this any more, pauses based on the number of API calls

  for (i in 1:nrow(api_tbl)) {

    if (use_pb) {setTxtProgressBar(pb, i)}

    ## Check if we need to do a pause
    if (i %% pause_n == 0) {
      if (!quiet) message(accent2(paste0(" - pausing for ", pause_secs, " seconds to let the server catch its breath!")))
      Sys.sleep(pause_secs)
    }

    ## First order of business - see if this API call has already been made
    hash_int <- api_tbl[i, "hash_int", drop = TRUE]
    hash_in_db <- hash_int %fin% hashes_already_in_db

    if (new_recs_only) {
      make_the_call <- !hash_in_db
      if (debug && !make_the_call) message(silver(paste0(" - api call already run, skipping")))
    } else {
      make_the_call <- TRUE
    }

    ## Next, call the API
    if (make_the_call) {

      if (aoi_sf) {

        ## Get the temporary geojson file name for this row (which is stored in the loc_qry column)
        gjsn_fn <- gjsn_all_fn[api_tbl[i, "loc_qry", drop = TRUE]]

        #gjsn_fn <- file.path(tempdir(), api_tbl[i, "loc_preset", drop = TRUE])

        if (!file.exists(gjsn_fn)) {
          if (debug) message(silver(paste0(" - saving temp geojson: ", basename(gjsn_fn))))
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
          message(silver(paste0(" - (", i, "/", nrow(api_tbl), ") PUT ", post_url, ". (", feat_id_fldname, "=",
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

        if (debug) message(silver(paste0(" - (", i, "/", nrow(api_tbl), ") ", api_url_full)))

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
            ## quiet = FALSE or debug = TRUE
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

            ## Get the field type for the feat_id column
            if (is.integer(api_tbl[[feat_id_fldname]])) {
              feat_id_type <- "INTEGER"
            } else if (is.numeric(api_tbl[[feat_id_fldname]])) {
              feat_id_type <- "NUMERIC"
            } else if (is.character(api_tbl[[feat_id_fldname]])) {
              feat_id_type <- "TEXT"
            } else {
              stop("DONT KNOW THE FIELD TYPE OF THE FEAT_ID COLUMN IN API_TBL")
            }

            if (lookup_tbls) {

              ## If using lookup tables, we're going to 'pre-define' the values table prior to writing
              ## data, so that we can create foreign keys

              ## Start the list of field definitions with the feat_id column
              sql_flds <- paste0(feat_id_fldname, " ", feat_id_type, " NOT NULL")

              ## Initialize a variable to hold expressions that will create the indices
              sql_indices <- character(0)

              ## Add an index expression for the feature id
              if ("feat_id" %in% indices || feat_id_fldname %in% indices) {
                sql_indices <- c(sql_indices,
                                 paste0("CREATE INDEX `", db_tbl, ".", feat_id_fldname,
                                        "` ON `", db_tbl, "`(`", feat_id_fldname, "`);"))
              }

              ## Define the lookup tables that are needed
              if (identical(x$slug, NA)) {
                flds_make <- c("cvar", "gcm", "scenario", "period", "spag")
              } else {
                flds_make <- c("slug", "spag")
              }

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
              sql_flds <- c(sql_flds, "dt TEXT NOT NULL", "val NUMERIC NOT NULL")   ## could also be REAL

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
          if (debug) message(silver(" - no values returned!"))
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
      if (debug) message(silver(paste0(" - deleting ", length(tmp_jsn_fn), " temp geojson files")))
      unlink(tmp_jsn_fn)
    }
  }

  ## Return a remote tibble
  if (is.na(tbl_sql) || !lookup_ret_joined) {
    res <- tbl(mydb, db_tbl)
  } else {
    if (debug) message(silver(paste0(" - returning a remote tbl based on SQL: \"", tbl_sql, "\"")))
    res <- tbl(mydb, sql(tbl_sql))
  }

  class(res) <- c(class(res), "ca_db")
  attr(res, "lkp_sql") <- tbl_sql
  attr(res, "vals_tbl") <- db_tbl
  invisible(res)

}


