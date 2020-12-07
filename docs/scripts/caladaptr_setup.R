# install.packages("easypackages")
#
# library(easypackages)
#
# # install.packages("pacman")
# # library(pacman)

req_pkg <- c("assertthat", "backports", "chillR", "conflicted","crayon", "curl", "DBI",
             "dbplyr", "digest", "dplyr", "fastmatch", "ggplot2", "httr", "lubridate",
             "magrittr", "purrr", "remotes", "RSQLite", "sf", "stars", "stringr",
             "tibble", "tidyr", "tmap", "units")

install.packages(setdiff(req_pkg, rownames(installed.packages())))


# p_install(req_pkg, character.only = TRUE, force = FALSE)
#
# p_install(tibble, tidyr, tmap, units, force = FALSE)
#
# packages()
