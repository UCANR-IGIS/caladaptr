#' Converts a Cal-Adapt values list to a data frame
#'
#' @param x A Cal-Adapt Values Object
#' @param feat_id Include feature id as a column
#' @param dt Include date
#' @param cvar Include climate variable
#' @param period Include period
#' @param gcm Include gcm
#' @param scenario Include scenario
#' @param spatial_ag Include spatial aggregation function
#' @param force_day_one Convert the dt to an integer value when period = year
#'
#' @details If \code{force_day_one = TRUE}, the day value of the dt column will be forced to January 1 when the
#' queried layer is pre-aggregated by year (i.e., \code{period = 'year'}). This ad-hoc conversion helps provide
#' consistency in how the 'date' of a temporally aggregated layer is returned by the Cal-Adapt events and
#' rasters end point. Without this conversion, the 'date' of a year may be returned as December 31.
#'
#' @seealso
#' \code{\link{ca_getvals}}
#'
#' @importFrom crayon bold yellow red
#' @importFrom units set_units
#' @importFrom tibble as_tibble
#' @export

ca_vals2tbl <- function(x, feat_id = TRUE, dt = TRUE, cvar = TRUE, period = TRUE, gcm = TRUE,
                        scenario = TRUE, spatial_ag = TRUE, force_day_one = TRUE) {

  if (!inherits(x, "ca_qryvals_lst")) stop("x should be class ca_qryvals_lst")

  ## Create a blank object to append the individual data frames to
  res_df <- NULL

  ## Start the big nested loop
  for (myfeatid in names(x)) {

    for (mycvar in names(x[[myfeatid]])) {

      for (myper in names(x[[myfeatid]][[mycvar]])) {

        for (mygcm in names(x[[myfeatid]][[mycvar]][[myper]])) {

          for (myscenario in names(x[[myfeatid]][[mycvar]][[myper]][[mygcm]])) {

            for (myspag in names(x[[myfeatid]][[mycvar]][[myper]][[mygcm]][[myscenario]])) {

              ## Grab this current data frame (two columns - dt & val)
              this_df <- x[[myfeatid]][[mycvar]][[myper]][[mygcm]][[myscenario]][[myspag]]

              ## Construct an expression that will generate the data frame with the correct columns
              feat_str <- ifelse(feat_id, paste0(", feat_id = factor(\"", myfeatid, "\")"), "")
              cvar_str <- ifelse(cvar, paste0(", cvar = factor(\"", mycvar, "\")"), "")
              per_str <- ifelse(period, paste0(", period = factor(\"", myper, "\")"), "")
              scenario_str <- ifelse(scenario, paste0(", scenario = factor(\"", myscenario, "\")"), "")
              gcm_str <- ifelse(gcm, paste0(", gcm = factor(\"", mygcm, "\")"), "")

              ## If annual data, convert dt to just the year (as an integer)
              ## ABANDONED - THIS COULD CAUSE PROBLEMS IN THE DT COLUMN IF THE QUERY INCLUDED
              ## BOTH YEAR AND DAY VALUES
              ## dt_str <- ifelse(dt, paste0(", dt = as.integer(substr(this_df$dt,1,4))"), "")

              if (myper == "year" && force_day_one) {
                dt_str <- ifelse(dt, paste0(", dt = as.Date(gsub(\"12-31$\", \"01-01\", this_df$dt))"), "")
              } else {
                dt_str <- ifelse(dt, paste0(", dt = as.Date(this_df$dt)"), "")
              }

              #cat(dt_str, "\n")

              ## dt_str <- ifelse(dt, paste0(", dt = as.Date(this_df$dt)"), "")

              spag_str <- ifelse(spatial_ag, paste0(", spatial_ag = factor(\"", myspag, "\")"), "")
              makedf_str <- paste0("data.frame(val = this_df$val", spag_str, dt_str, scenario_str, gcm_str,
                                   per_str, cvar_str, feat_str, ")")
              makedf_exp <- parse(text = makedf_str)

              ## Evaluate the expression to generate a data frame with additional columns desired



              allcols_df <- eval(makedf_exp)

              ## Append these rows
              res_df <- rbind(res_df, allcols_df)

            }
          }
        }
      }
    }

  }

  ## Return the data frame
  if (is.null(res_df)) {
    NULL

  } else {
    ## Restore the name and class of the feat_id column
    if (feat_id && !is.null(attr(x, "idfld"))) {
      featid_orig_prp <- attr(x, "idfld")
      featid_colidx <- which(names(res_df) == "feat_id")
      names(res_df)[featid_colidx] <- featid_orig_prp$name
      if (class(res_df[[featid_colidx]]) !=  featid_orig_prp$class) {
        ## Have to wrap the current feat_id values as character before as(x, "")
        ## because they are likely to be factors
        res_df[[featid_colidx]] <- as(as.character(res_df[[featid_colidx]]), featid_orig_prp$class)
      }
    }

    ## Change the day to January 1 when per=year
    ## MOVED TO THE INNER LOOP BECAUSE PERIOD MAY NOT BE AVAILABLE DOWN HERE
    # if (dt) {
    #   browser()
    #
    #   if (force_day_one) {
    #     dim(res_df); head(res_df)
    #     class(res_df$dt)
    #
    #     year_idx <-
    #
    #   }
    #
    # }

    ## Convert the dt column to a date - MOVED TO THE INNER LOOPS
    ## res_df[ , "dt"] <- as.Date(res_df[ , "dt"] )

    ## Return the result, flipping the column order
    as_tibble(res_df[ , length(res_df):1])
  }

}

