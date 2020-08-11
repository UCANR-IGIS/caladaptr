#' Check the response to an API call
#'
#' Check the response to an API call
#'
#' @param resp A response object
#' @param task_msg A message describing the API task
#'
#' @importFrom assertthat assert_that
#' @importFrom httr warn_for_status

ca_resp_check <- function(resp, task_msg) {

  if (!inherits(resp, "response")) stop("resp should be of class \"response\"")

  ## Trap response problems

  ## Stop everything for a return code >= 400
  msg_code400 <- "Server returned an error code. Check your request."
  assert_that(resp$status_code < 400, msg = msg_code400)

  ## Give a warning for other things
  warn_for_status(resp, task_msg)

}
