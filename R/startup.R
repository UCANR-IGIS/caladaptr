.onAttach <- function(lib, pkg) {
    ver <- read.dcf(file.path(lib,pkg,"DESCRIPTION"), "Version")
    msg <- sprintf("Cal-Adapt R (version %s)\nURL: https://github.com/ucanr-igis/caladaptr\nBug reports: caladaptr@gmail.com", as.character(ver))
    packageStartupMessage(msg)
}
