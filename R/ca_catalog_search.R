#' Search the Cal-Adapt raster series data catalog
#'
#' Search the Cal-Adapt raster series data catalog
#'
#' @param x text to search for
#' @param keep_together treat x as a phrase, logical
#' @param quiet suppress messages
#'
#' @details This function can be used to search the local copy of the Cal-Adapt raster series
#' data catalog, and view the properties of the matching results. Searched fields include the
#' dataset name and slug. If \code{keep_together = TRUE}, the search text will be treated
#' as a phrase, otherwise the words in x will be searched for separately.
#' Records have to match all terms to be returned.
#'
#' For an online search tool, click the 'Filters' button on
#' \url{https://api.cal-adapt.org/api/series/}.
#'
#' @return A tibble with information about the found raster series
#'
#' @seealso \code{\link{ca_catalog_rs}}, \code{\link{ca_catalog_fetch}}
#'
#' @importFrom dplyr filter relocate
#' @importFrom magrittr extract2
#'
#' @examples
#' \dontrun{
#' ## Search for a slug
#' ca_catalog_search("pr_day_gridmet")
#'
#' ## Search for keywords
#' ca_catalog_search("evapotranspiration year")
#'
#' ## Search for phrase
#' ca_catalog_search("Livneh VIC", keep_together = TRUE)
#' }
#'
#' @export

ca_catalog_search <- function(x, keep_together = FALSE, quiet = FALSE) {

  if (length(x) != 1) stop("x should be a search term of length 1")

  rs_tbl <- ca_catalog_rs(quiet = TRUE)
  search_me <- paste(rs_tbl$name, rs_tbl$slug, sep = " ")

  if (keep_together) {
    search_terms <- x
  } else {
    search_terms <- x %>%
      gsub(",", " ", .) %>%
      strsplit(., " ") %>%
      extract2(1) %>%
      trimws()
  }

  ## Create a logical vector for the results, starting with all TRUE
  match_yn <- rep(TRUE, length(search_me))

  ## Loop through the terms
  for (find_me in search_terms) {
    match_yn <- match_yn & grepl(find_me, search_me, ignore.case = TRUE)
  }

  ## Search the slug column in the raster series catalog
  search_results_tbl <- rs_tbl %>%
    filter(match_yn) %>%
    relocate(slug, name, url, tres, begin, end, units, num_rast, id, xmin, xmax, ymin, ymax)

  ## Print results
  if (!quiet) {

    ## Get the functions to format messages
    accent1 <- getOption("ca_accent1", I)
    accent2 <- getOption("ca_accent2", I)
    msg_fmt <- getOption("ca_message", I)

    if (nrow(search_results_tbl) > 0) {
      for (i in 1:nrow(search_results_tbl)) {
        for (j in 1:ncol(search_results_tbl)) {
          if (j == 1) {
            message(accent1(paste0("\n", search_results_tbl[i,j,drop=T])))
          } else {
            message(accent2(paste0("  ", names(search_results_tbl)[j], ": ")),
                    msg_fmt(search_results_tbl[i,j,drop=T]))
          }
        }
      }

    } else {
      message(accent2("No matching raster series found"))
    }
  }

  invisible(search_results_tbl)

}
