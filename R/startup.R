.onAttach <- function(lib, pkg) {
    ver <- read.dcf(file.path(lib,pkg,"DESCRIPTION"), "Version")

    bug_reports <- read.dcf(file.path(lib,pkg,"DESCRIPTION"), "BugReports")
    package_url <- read.dcf(file.path(lib,pkg,"DESCRIPTION"), "URL")
    msg <- paste0(sprintf("caladaptr (version %s)", as.character(ver)), "\n",
                  "URL: ", package_url, "\n",
                  "Bug reports: ", bug_reports)
    packageStartupMessage(msg)
}
