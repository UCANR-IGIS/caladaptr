#' Write values from an API request to a local database
#'
#' @param x A Cal-Adapt API request
#' @param db_fn File name of a SQLite database. See Details.
#' @param db_tbl The name of a database table. See Details.
#' @param trans_len Number of APIs calls per write transaction. See Details.
#' @param indices Not used yet
#' @param new_recs_only Write new records only to the database. See Details.
#' @param lookup_tbls Use lookup tables
#' @param pause_n Number of values after which a built-in pause is triggered. See Details.
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
#' added to the database. \code{trans_len} defines the number of API calls per transaction (i.e. how many API calls of data to
#' accumulate before doing a write operation to the database). This can speed things up. Set it to 0 to disable transactions.
#'
#' \code{pause_n} is the number of values after which a built-in pause of length \code{pause_secs} is triggered. This is intended
#' to avoid disruption on the Cal-Adapt server (specifically cache management).
#'
#' The returned tibble is linked to the SQLite datbase. For the most part you can use the same dplyr functions to manipulate
#' the results, but to retrieve the actual values you need to use `collect()`. For more info working with a linked database,
#' see \url{https://dbplyr.tidyverse.org/articles/dbplyr.html}.
#'
#' @return A tibble linked to the SQLite database.
#'
#' @importFrom crayon bold yellow red silver green magenta
#' @importFrom httr GET content content_type_json modify_url
#' @importFrom utils txtProgressBar setTxtProgressBar packageVersion
#' @importFrom dplyr select mutate left_join tbl
#' @importFrom curl has_internet
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable dbListTables dbCreateTable dbReadTable dbExecute dbIsValid
#' @importFrom RSQLite SQLite dbBegin dbCommit
#' @importFrom fastmatch `%fin%`
#' @export

ca_getvals_db <- function(x, db_fn, db_tbl, indices = NULL, new_recs_only = TRUE,
                          trans_len = 100, lookup_tbls = TRUE,
                          pause_n = 1000000, pause_secs = 60,
                          stop_on_err = TRUE,
                          quiet = FALSE, debug = TRUE) {

  if (!inherits(x, "ca_apireq")) stop("x should be a ca_apireq")

  ## Check for an internet connection
  if (!has_internet()) stop("No internet connection detected")

  ## Get a tibble with the individual API calls
  apicalls_lst <- ca_apicalls(x)
  api_tbl <- apicalls_lst$api_tbl

  ## Get the name of first column and random string to make the geojson filenames unique to this run
  feat_id_fldname <- apicalls_lst$idfld
  gson_fn_base  <- apicalls_lst$gson_fn_base

  ## Rename the first column (feature id)
  names(api_tbl)[1] <- feat_id_fldname

  if (length(unique(api_tbl$rs_units)) > 1) {
    stop("Can not process this API request: Raster series have different units")
  }

  ## Define which columns to save in the output table (either in-memory tibble or db)
  if (identical(x$slug, NA)) {

    ## API Request is based on gcm+scenario+cvar+period
    if (lookup_tbls) {

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

    } else {
      ## User wants an in-memory tibble --> do not use lookup ids
      apicall_cols_keep <- c(names(api_tbl)[1], "cvar", "period", "gcm", "scenario", "spag")

    }


  } else {
    ## API request is based on slug(s)
    #message(silver(" - still need to create a lookup table for all slugs"))
    apicall_cols_keep <- c(names(api_tbl)[1], "slug", "spag")
  }


  ## Close any open connections leftover from previous function calls
  ## that were interrupted.
  ## NOT SURE IF THIS IS NEEDED, GETTING INCONSISTENT RESULTS
  ## IF I DON'T KEEP THIS REMOVE dbIsValid FROM IMPORTS
  if (!is.null(getOption("ca_dbconn"))) {

    if (dbIsValid(getOption("ca_dbconn"))) {
      if (debug) message(yellow(" - Found a leftover open database connection, will try to close it"))
      dbDisconnect(getOption("ca_dbconn"))
    }

    # else {
    # if (debug) message(yellow(" - Found a leftover connection but it isn't valid, ignoring"))

    options("ca_dbconn" = NULL)
  }

  #####################################################################
  ## Create the sqlite database and various tables if needed
  use_db <- !is.null(db_fn)

  ## Get the table name
  db_tbl_use <- db_tbl

  ## Create a connection and enable referential integrity (for this connection only, its not persistent)
  mydb <- dbConnect(SQLite(), db_fn)

  ## Set up auto-disconnect - NEW THINKING - DONT WANT TO DO THIS
  #on.exit(dbDisconnect(mydb))

  ## Record this connection in an option in case there is an unexpected termination
  options("ca_dbconn" = mydb)

  ## on.exit(options("ca_dbconn" = NULL))

  ## Enforce referential integrity (this connection only)
  dbExecute(conn = mydb, "PRAGMA foreign_keys = ON")

  if (debug) message(silver(paste0(" - new values will be saved to `", db_tbl_use, "` in: ", db_fn)))

  ## Get all tables currently in the database
  all_tbls <- dbListTables(mydb)

  ## Construct the name of the table that saves processed api_urls
  db_hashes_tbl <- paste0(db_tbl_use, "_hashes")

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

  # if (debug) message(red("FOUND THESE HASHES (SHOULD BE INTEGER)"))
  # if (debug) print(hashes_already_in_db)

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
                       value = get( paste0(lkp, "_tbl")),
                       append = TRUE)

          if (debug) message(silver(paste0(" - created lookup table: ", lkp, "s")))

        }

      }

      ## At this point all the lookup tables should be in the database!!
      # browser()
      # dbListTables(mydb)
      # message(yellow("At this point all the lookup tables should be in the database!!"))
      # res <- dbSendQuery(mydb, "PRAGMA foreign_keys")
      # message(yellow(paste0("foreign_keys = ", dbFetch(res)[1,1])))
      # dbClearResult(res)


    } else {
      #message(yellow("Need to create the lookup table for slug"))
      ## Create the lookup tables for slug and spag

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
    gjsn_all_fn <- file.path(tempdir(),
                             paste0("~ca_", gson_fn_base,
                                    sprintf("_%03d", 1:nrow(apicalls_lst$loc_sf)),
                                    ".geojson"))
  }

  ## If a progress bar is needed, set it up
  use_pb <- !quiet && !debug && (nrow(api_tbl) > 3)
  if (use_pb) pb <- txtProgressBar(min = 1, max = nrow(api_tbl), style = 3)

  ## Loop through calls
  if (debug) message(silver(paste0(" - going to make ", nrow(api_tbl), " api calls")))

  num_vals_downloaded <- 0

  for (i in 1:nrow(api_tbl)) {

    if (use_pb) {setTxtProgressBar(pb, i)}

    ## First order of business - see if this API call has already been made
    hash_int <- api_tbl[i, "hash_int", drop = TRUE]
    hash_in_db <- hash_int %fin% hashes_already_in_db

    if (new_recs_only) {
      make_the_call <- !hash_in_db
      if (debug && !make_the_call) message(magenta(paste0(" - API call already run, skipping")))
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
                              stat = spatial_ag,
                              format = "json")

        post_url <- api_tbl[i, "api_url", drop = TRUE]

        if (debug) {
          feat_id <- api_tbl[i, 1, drop = TRUE]
          message(silver(paste0(" - (", i, "/", nrow(api_tbl), ") PUT ", post_url, ". (", feat_id_fldname, "=",
                                feat_id, ", start='", start_dt, "', end='", end_dt, "', stat='", spatial_ag, "', format='json')")))
        }

        ## Make the POST request
        qry_resp <- POST(url = post_url,
                         body = body_flds_lst,
                         encode = "multipart",
                         caladaptr_ua)

        # if (qry_resp$status_code == 200) {
        #
        #   ## Convert response to a list
        #   qry_content <- content(qry_resp, type = "application/json")
        #
        #   if (length(qry_content) > 0) {
        #
        #     ## Get the data values (one per date)
        #     these_vals <- unlist(qry_content$data)
        #
        #     ## Convert units
        #     if (!is.na(api_tbl[i, "rs_units", drop = TRUE])) {
        #       these_vals <- set_units(these_vals,
        #                               value = api_tbl[i, "rs_units", drop = TRUE],
        #                               mode = "standard")
        #     }
        #
        #     ## Append these rows to the tibble
        #     res_tbl <- rbind(res_tbl,
        #                      tibble(api_tbl[i, apicall_cols_keep],
        #                             dt = substr(unlist(qry_content$index), 1, 10),
        #                             val = these_vals))
        #
        #   } else {
        #     ## Nothing returned - date fell outside of range? location outside extent?
        #     if (debug) message(silver(" - no values returned!"))
        #   }
        #
        #
        # } else {
        #   if (debug) message(red(paste0(" - Oh dear. Status code = ", qry_resp$status_code)))
        #   if (stop_on_err) ca_resp_check(qry_resp, "POST request to query a raster")
        # }

        ## END IF AOI_SF
        ###########################################


      } else {   ## not aoi_sf

        api_url_full <- api_tbl[i, "api_url", drop = TRUE]
        #api_url_short <- as.character(substring(api_url_full, 30, nchar(api_url_full)))

        if (debug) message(silver(paste0(" - (", i, "/", nrow(api_tbl), ") ", api_url_full)))

        ## Make request
        qry_resp <- GET(api_url_full, content_type_json(), caladaptr_ua)

        # if (qry_resp$status_code == 200) {
        #
        #   ## Convert response to a list
        #   qry_content <- content(qry_resp, type = "application/json")
        #
        #   if (length(qry_content) > 0) {
        #
        #     ## Get the data values (one per date)
        #     these_vals <- unlist(qry_content$data)
        #
        #     ## Convert units
        #     if (!is.na(api_tbl[i, "rs_units", drop = TRUE])) {
        #       these_vals <- set_units(these_vals,
        #                               value = api_tbl[i, "rs_units", drop = TRUE],
        #                               mode = "standard")
        #     }
        #
        #     ## Append these rows to the tibble
        #     res_tbl <- rbind(res_tbl,
        #                      tibble(api_tbl[i, apicall_cols_keep],
        #                             dt = substr(unlist(qry_content$index), 1, 10),
        #                             val = these_vals))
        #
        #   } else {
        #     ## Nothing returned - date fell outside of range? location outside extent?
        #     if (debug) message(silver(" - no values returned!"))
        #   }
        #
        # } else {
        #
        #   if (debug) message(red(paste0(" - Oh dear. Status code = ", qry_resp$status_code)))
        #   if (stop_on_err) ca_resp_check(qry_resp, "Retrieve pixel values")
        # }


      }   ## else a non-sf query


      ######################################################################
      ## AT THIS POINT, WE HAVE RESPONSE

      if (qry_resp$status_code == 200) {

        ## Call was successful - convert response content to a list
        qry_content <- content(qry_resp, type = "application/json")

        if (length(qry_content) > 0) {

          ## Get the data values (one per date)
          these_vals <- unlist(qry_content$data)

          ## Check if we need to do a pause
          num_vals_downloaded <- num_vals_downloaded + length(these_vals)
          if (num_vals_downloaded > pause_n) {
            if (!quiet) message(yellow$bold(paste0(" - pausing for ", pause_secs, " seconds to let the server catch its breath!")))
            Sys.sleep(pause_secs)
            num_vals_downloaded <- 0
          }

          ## Not converting to units object because that isn't compatible with SQLite

          ## Time to write out some data!! First see if the 'values' table has been created or not.
          new_tbl <- !db_tbl_use %in% dbListTables(mydb)

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

              if (identical(x$slug, NA)) {

                ## Start the list of field definitions with the feat_id column
                sql_flds <- paste0(feat_id_fldname, " ", feat_id_type, " NOT NULL")

                sql_indices <- character(0)

                ## Start the list of indices with feat_id column (which we definitely want to index)
                if ("feat_id" %in% indices) {
                  sql_indices <- c(sql_indices,
                                   paste0("CREATE INDEX val_", feat_id_fldname,
                                          " ON `", db_tbl_use, "`(`", feat_id_fldname, "`);"))
                }

                ## Add field definition and index creation expressions for other fields
                for (fld in c("cvar", "gcm", "scenario", "period", "spag")) {

                  sql_flds <- c(sql_flds,
                                paste0(fld, "_id INTEGER NOT NULL REFERENCES ", fld, "s"))

                  if (fld %in% indices) {
                    sql_indices <- c(sql_indices,
                                     paste0("CREATE INDEX val_", fld, "_id ON `", db_tbl_use, "`(", fld, "_id);"))

                  }

                }

                ## Add additional field definitions for dt and val (no indices needed)
                sql_flds <- c(sql_flds, "dt TEXT NOT NULL")
                sql_flds <- c(sql_flds, "val NUMERIC NOT NULL")  ## could also be REAL

                ## Create the table
                sql_create_my_table <- paste0("CREATE TABLE `", db_tbl_use, "` (",
                                              paste(sql_flds, collapse = ", "), ");")
                dbExecute(mydb, sql_create_my_table)

                # Generate indices for the foreign key fields in the vals table
                for (sql_create_index in sql_indices) {
                  dbExecute(mydb, sql_create_index)
                }

              }

            }

          }

          ## Write the data. This statement works regardless of using lookup tables or not
          dbWriteTable(mydb, name = db_tbl_use,
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

          # write_hash_int <- new_recs_only
          # if (!new_recs_only && (hash_int %fin% hashes_already_in_db)) {
          #   write_hash_int <- FALSE
          # }
          # if (write_short_url) {
          #   dbWriteTable(mydb, name = db_hashes_tbl,
          #                value = data.frame(url_short = api_url_short),
          #                append = TRUE)
          # }


          ## Commit the transaction every nth call
          if (trans_len > 0) {
            calls_made <- calls_made + 1
            if (calls_made %% trans_len == 0) {
              ##dbExecute(mydb, "COMMIT;")
              dbCommit(mydb)
              if (debug) message(yellow(paste0(" - COMMIT. ", num_vals_downloaded, " values downloaded since last pause point.")))
              #if (debug) message(yellow(paste0(" - ", )))

              ## Start a new transaction
              dbBegin(mydb)

            }
          }

        } else {
          ## Nothing returned - date fell outside of range? location outside extent?
          if (debug) message(silver(" - no values returned!"))
        }

      } else {

        if (debug) message(red(paste0(" - Oh dear. Status code = ", qry_resp$status_code)))
        if (stop_on_err) ca_resp_check(qry_resp, "POST request to query a raster")

      } ## if (qry_resp$status_code == 200)


    }   ## if make_the_call








    # #### BELOW HERE IS OLD STUFF
    #
    # api_url_full <- api_tbl[i, "api_url", drop = TRUE]
    # api_url_short <- as.character(substring(api_url_full, 30, nchar(api_url_full)))  ## unique identifier for search
    #
    # ## Define a unique id that uniquely identifies this call
    # if (aoi_sf) {
    #
    #   ## Create values for parameters (setting them to NULL if not used)
    #   if (is.na(api_tbl[i, "start", drop = TRUE])) {
    #     start_dt <- NULL
    #   } else {
    #     start_dt <- as.character(api_tbl[i, "start", drop = TRUE])
    #   }
    #
    #   if (is.na(api_tbl[i, "end", drop = TRUE])) {
    #     end_dt <- NULL
    #   } else {
    #     end_dt <- as.character(api_tbl[i, "end", drop = TRUE])
    #   }
    #
    #   if (is.na(api_tbl[i, "spag", drop = TRUE]) || api_tbl[i, "spag", drop = TRUE] == "none") {
    #     spatial_ag <- NULL
    #   } else {
    #     spatial_ag <- as.character(api_tbl[i, "spag", drop = TRUE])
    #   }
    #
    #   api_url_short <- "12345"
    #
    #   api_hash <- paste(api_url_short, start_dt, end_dt, spatial_ag, sep = ".")
    #
    #   # message(silver(paste0(" - ", i, "/", nrow(api_tbl), ") PUT ", post_url, ". (", feat_id_fldname, "=",
    #   #                       feat_id, ", start='", start_dt, "', end='", end_dt, "', stat='", spatial_ag, "', format='json')")))
    #
    #
    #   ## need to hash his search
    #   ## # perhaps paste the slug, dates, spag, idfld, idval, and dTolerance, then run it through a digest::digest()
    #
    # } else {
    #   if (debug) message(silver(paste0(" - (", i, "/", nrow(api_tbl), ") ", api_url_full)))
    #   api_hash <- api_url_short
    # }


    # if (new_recs_only) {
    #   ## See if this URL is already in the database
    #   make_the_call <- !api_hash %fin% hashes_already_in_db
    #
    #   if (debug && !make_the_call) message(magenta(paste0(" - API call already run, skipping")))
    #
    # } else {
    #   make_the_call <- TRUE
    # }

    # if (make_the_call) {
    #
    #   if (aoi_sf) {
    #
    #     ## This is where the bulk of the calling code goes
    #
    #     browser()
    #
    #     gjsn_fn <- file.path(tempdir(), api_tbl[i, "loc_preset", drop = TRUE])
    #     if (!file.exists(gjsn_fn)) {
    #       if (debug) message(silver(paste0(" - saving ", api_tbl[i, "loc_preset", drop = TRUE])))
    #       st_write(apicalls_lst$loc_sf %>%
    #                  slice(api_tbl[i, "loc_qry", drop = TRUE]),
    #                dsn = gjsn_fn,
    #                quiet = TRUE)
    #     }
    #
    #
    #     body_flds_lst <- list(features = upload_file(gjsn_fn),
    #                           start = start_dt,
    #                           end = end_dt,
    #                           stat = spatial_ag,
    #                           format = "json")
    #
    #     post_url <- api_tbl[i, "api_url", drop = TRUE]
    #
    #     if (debug) {
    #       feat_id <- api_tbl[i, 1, drop = TRUE]
    #       message(silver(paste0(" - (", i, "/", nrow(api_tbl), ") PUT ", post_url, ". (", feat_id_fldname, "=",
    #                             feat_id, ", start='", start_dt, "', end='", end_dt, "', stat='", spatial_ag, "', format='json')")))
    #     }
    #
    #     ## Make the POST request
    #     qry_resp <- POST(url = post_url,
    #                      body = body_flds_lst,
    #                      encode = "multipart",
    #                      caladaptr_ua)
    #
    #     ca_resp_check(qry_resp, "POST request to query a raster")
    #
    #   } else {
    #
    #     ## NOT AOI_SF Make request
    #     qry_resp <- GET(api_url_full, content_type_json(), caladaptr_ua)
    #     ca_resp_check(qry_resp, "GET request to query a raster")
    #
    #   }
    #
    # }


  }  ## done with the loop  for (i in 1:nrow(api_tbl))

  ## Close the progress bar
  if (use_pb) close(pb)

  ## Commit the final write
  if (trans_len > 0 && calls_made > 0) {
    dbCommit(mydb)
    if (debug) message(yellow(" - FINAL COMMIT"))
  }

  ## No need to close the DBI connection, need to leave it open for the tibble we return to work

  ## Delete any temporary geojson files created
  if (aoi_sf) {
    tmp_jsn_fn <- list.files(tempdir(), pattern = "^\\~ca_(.*).geojson$", full.names = TRUE)
    if (length(tmp_jsn_fn) > 0) {
      if (debug) message(silver(paste0(" - deleting ", length(tmp_jsn_fn), " temp geojson files")))
      unlink(tmp_jsn_fn)
    }
  }

  ## Return a tibble
  res <- tbl(mydb, db_tbl_use)
  class(res) <- c(class(res), "ca_db")
  invisible(res)

}


