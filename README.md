
<!-- README.md is generated from README.Rmd. Please edit that file -->

# caladaptR <img src="man/figures/caladaptr-beta_logo.svg" align="right" width="240" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

See also: <https://ucanr-igis.github.io/caladaptr/>

`caladaptr` is an API client that makes it easier to work with data from
[Cal-Adapt.org](https://cal-adapt.org/) in R. The role of `caladaptr` is
to bring data into R and provide low-level functions to get it into the
shape and format you require:

<p align="center">

<img src="man/figures/caladaptr_workflow_630x524x256.png" width="504" height="419"/>

</p>

  

**‘Beta’** status means:

1)  the package is still under development  
2)  the package is being updated fairly often  
3)  there’s a possibility that updates will *not* be backward
    compatible  
4)  user feedback and input is extremely welcome\! (please join the
    [caladaptR betaR](#caladaptr-betar-club) club)

*Development Status (Aug 2020)*

  - `caladaptr` only supports Cal-Adapt’s raster data layers (which is
    most of them). There are no plans at present to support importing
    station data (i.e., sea level rise).  
  - Currently only temperature and precipitation rasters can be queried
    (both projected and historic modeled). Variables from the VIC
    datasets and others will be coming soon.  
  - Retrieving values currently works. Retrieving rasters is not yet
    supported.  
  - Currently you can query point locations and preset areas of
    interest. Querying a user-provided polygon will be coming soon.

## Installation

`caladaptr` is hosted on
[GitHub](https://github.com/ucanr-igis/caladaptr). To install it, you
need to have [RTools](https://cran.r-project.org/bin/windows/Rtools/)
and the `devtools` (or `remotes`) package installed:

``` r
library(devtools)
devtools::install_github("ucanr-igis/caladaptr")
```

## General Workflow

In general, there are three steps to getting data via the Cal-Adapt API:

1)  Create a ‘Cal-Adapt API Request’ object

2)  Feed the API Request object into a function that fetches data
    (either values or rasters\*)

3)  Massage the data that comes back into the format you require (e.g.,
    a data frame or clipped raster)

## Sample Usage: Get Annual Projected Temperature at a Point Location

1)  Load the package:

<!-- end list -->

``` r
library(caladaptr)
#> Cal-Adapt R (version 0.2.9)
#> URL: https://github.com/ucanr-igis/caladaptr
#> Bug reports: caladaptr@gmail.com
```

2)  Next, create an **API request object** for a point location. This
    doesn’t actually fetch any data, it puts together the pieces pieces
    of an API request. There are a number of ‘*constructor*’ functions
    you can mix and match to create an API request object, for example:

<!-- end list -->

``` r
cap1 <- ca_loc_pt(coords = c(-121.4687, 38.5938)) %>%  ## specify a location
  ca_gcm(gcms[1:4]) %>%                                ## select GCM(s)
  ca_scenario(scenarios[1:2]) %>%                      ## select emission scenarios(s)
  ca_period("year") %>%                                ## select a precooked temporal aggregation period
  ca_years(start = 2040, end = 2060) %>%               ## select start and end dates
  ca_cvar(c("tasmax", "tasmin"))                       ## select climate variables

cap1
#> Cal-Adapt API Request
#> Location(s): 
#>   x: -121.469
#>   y: 38.594
#> Variable(s): tasmax, tasmin
#> Temporal aggregration period(s): year
#> GCM(s): HadGEM2-ES, CNRM-CM5, CanESM2, MIROC5
#> Scenario(s): rcp45, rcp85
#> Dates: 2040-01-01 to 2060-01-01
#> Options: NA
```

To help you pass arguments for the various constructor functions,
`caladpatr` provides the following constants:

``` r
climvars
#> [1] "tasmax" "tasmin" "pr"     "swe"

gcms
#>  [1] "HadGEM2-ES" "CNRM-CM5"   "CanESM2"    "MIROC5"     "ACCESS1-0" 
#>  [6] "CCSM4"      "CESM1-BGC"  "CMCC-CMS"   "GFDL-CM3"   "HadGEM2-CC"
#> [11] "ens32avg"   "ens32max"   "ens32min"

## Note the first four GCMs are the 'priority' models recommended under 
## California's 4th Climate Change Assessment.

scenarios
#> [1] "rcp45"      "rcp85"      "historical"

periods
#> [1] "day"    "month"  "year"   "30yavg"
```

**Note**: Cal-Adapt has data for many but by no means all permutations
of the above constants.

3)  Next, feed your API call into a function that **fetches data**, such
    as `ca_getvals()`.

<!-- end list -->

``` r
cap1_vals_df <- ca_getvals(cap1, quiet = TRUE) %>% ca_vals2tbl()
dim(cap1_vals_df)
#> [1] 336   7
head(cap1_vals_df)
#> # A tibble: 6 x 7
#>   feat_id cvar   period gcm        scenario dt              val
#>   <fct>   <fct>  <fct>  <fct>      <fct>    <chr>           [K]
#> 1 1       tasmax year   HadGEM2-ES rcp45    2040-01-01 298.7698
#> 2 1       tasmax year   HadGEM2-ES rcp45    2041-01-01 298.7150
#> 3 1       tasmax year   HadGEM2-ES rcp45    2042-01-01 298.5987
#> 4 1       tasmax year   HadGEM2-ES rcp45    2043-01-01 299.6766
#> 5 1       tasmax year   HadGEM2-ES rcp45    2044-01-01 299.5327
#> 6 1       tasmax year   HadGEM2-ES rcp45    2045-01-01 298.2451
```

For more examples, including retrieving data for a preset
area-of-interest (i.e., census tracts), see the ‘R Notebooks’ menu on
the [website](https://ucanr-igis.github.io/caladaptr/).

# Convenience Features and Functions

  - All `caladaptr` functions are pipe friendly and tidyverse
    compliant.  
  - Spatial data formats use the `sf` and `raster` packages.  
    \-Returned values have units (thanks to the
    [units](https://cran.r-project.org/package=units)) package. No more
    guessing what the units are or having to look up unit conversion
    constants\!  
  - Plotting an API Request object shows the query’s location.  
  - You can search the Cal-Adapt catalog of raster series from within R.
    Download a new version of the catalog with `ca_catalog_rs()`.  
  - Don’t need every single column in your output dataframe?
    `ca_vals2tbl()` provides arguments to specify which column(s) to
    return.

# caladaptR betaR club

The **caladaptR betaR** club is **now accepting members\!** If you would
like to be part of this elite cadre of `caladaptr` early adopters, you
should join our beta testing program. Benefits of joining the
**caladaptR betaR** club include:

  - early notification of **package updates**  
  - 24/7 **priority support**  
  - **exclusive invitations** to `caladaptr` tutorials and user feedback
    sessions
  - an **email forum** to bounce ideas around with other caladaptR users
    and the package developer

To join the **caladaptR betaR** club, please subscribe yourself to the
[caladaptR email
list](https://lists.ucdavis.edu/sympa/subscribe/caladaptr).
