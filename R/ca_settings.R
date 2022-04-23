#' Manage package settings
#'
#' View and manage package settings
#' @param console_colors The name of a preset, or list
#' @param date_slice Whether to use date slicing on the Cal-Adapt API when available
#' @param quiet Suppress messages, logical
#'
#' @details \code{console_colors} controls the color of text printed at the console. You can pass the name of a preset or a named list
#' of color functions (i.e., from crayon package). Up to six styles are recognized, see example below.
#'
#' \code{date_slice} determines whether or not date slicing via URL construction should be used for those Cal-Adapt datasets that
#' support it. This is generally a good idea, but can be set to FALSE for trouble-shooting.
#'
#' @seealso \code{\link{ca_getcache}}, \code{\link{ca_setcache}}
#'
#' @examples
#' \dontrun{
#' ca_settings(console_colors = list(ca_accent1 = crayon::blue$bold,
#'                                   ca_accent2 = crayon::magenta,
#'                                   ca_accent3 = crayon::red$bold,
#'                                   ca_accent4 = crayon::red,
#'                                   ca_message = crayon::silver,
#'                                   ca_success = crayon::green$bold))
#' }
#'
#' @import crayon
#' @export

ca_settings <- function(console_colors = NA, date_slice = NA, quiet = FALSE) {

  if (!missing(console_colors)) {

    if (identical(console_colors, "dark")) {
      options(ca_accent1 = crayon::yellow$bold)
      options(ca_accent2 = crayon::yellow)
      options(ca_accent3 = crayon::cyan$bold)
      options(ca_accent4 = crayon::cyan)
      options(ca_message = crayon::white)
      options(ca_success = crayon::green)

    } else if (identical(console_colors, "light")) {
      options(ca_accent1 = crayon::blue$bold)
      options(ca_accent2 = crayon::blue)
      options(ca_accent3 = crayon::magenta$bold)
      options(ca_accent4 = crayon::magenta)
      options(ca_message = crayon::silver)
      options(ca_success = crayon::green)

    } else {
      if (is.list(console_colors)) {
        do.call(options, console_colors)
      } else {
        stop("console_colors should the name of a preset (like 'dark' or 'light'), or a named list")
      }
    }

    if (!quiet) message(paste0("Console colors updated:\n",
                               getOption("ca_accent1", I)("  Accent 1"), "\n",
                               getOption("ca_accent2", I)("  Accent 2"), "\n",
                               getOption("ca_accent3", I)("  Accent 3"), "\n",
                               getOption("ca_accent4", I)("  Accent 4"), "\n",
                               getOption("ca_message", I)("  Message"), "\n",
                               getOption("ca_success", I)("  Success"), "\n"))
  }

  if (!missing(date_slice)) {
    options(ca_date_slice = date_slice)
    if (!quiet) message(paste0("Date slice updated to", date_slice))
  }

}

