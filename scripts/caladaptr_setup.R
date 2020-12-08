######################################################################
## caladaptR Workshop Setup
##
## Please run the following lines of code *before* the workshop starts
## to install all the packages that we'll be using during the workshop.
##
## If you have any difficulties please email the instructor.
##
## For more info about caladaptr, visit:
## https://ucanr-igis.github.io/caladaptr/
######################################################################

## Define the required packages (these are all on CRAN)

req_pkg <- c("assertthat", "backports", "chillR", "conflicted","crayon", "curl", "DBI",
             "dbplyr", "digest", "dplyr", "fastmatch", "ggplot2", "httr", "lubridate",
             "magrittr", "purrr", "remotes", "rmarkdown", "RSQLite", "sf", "stars", "stringr",
             "tibble", "tidyr", "tmap", "units")

## Install a fresh version of *all* required packages (recommended)

##  - if it asks you to restart R more than once, select 'no'.
##  - if it asks whether you want to install from source, select 'no'

install.packages(req_pkg, dependencies = TRUE)

## OR uncomment the next line to just install missing packages:
# install.packages(setdiff(req_pkg, rownames(installed.packages())), dependencies = TRUE)

## Install caladaptr
## (If it asks you whether you want to update a zillion packages, you can generally skip these
## unless its one of the above.)

remotes::install_github("ucanr-igis/caladaptr")

## Load it

library(caladaptr)

## See if it works

library(ggplot2); library(units); library(dplyr)

ca_example_apireq(1) %>%
  ca_getvals_tbl() %>%
  mutate(temp_f = set_units(val, degF)) %>%
  ggplot(aes(x = as.Date(dt), y = as.numeric(temp_f))) +
  geom_line(aes(color=gcm)) +
  labs(title = "Annual Max Temp for Sacramento", x = "year", y = "temp (F)")

## See a plot? Done!

