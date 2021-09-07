## This code is run before httptest is called
## This file used to be helper.R, but was changed to setup.R based on recommendations:
## https://testthat.r-lib.org/reference/test_dir.html#special-files

## The only disadvantage is that setup.R is not run by load_all, however in this case that's not a problem
## because all we're doing here is loading libraries

library(httptest)
library(sf)
library(dplyr)
library(DBI)
