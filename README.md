
<!-- README.md is generated from README.Rmd. Please edit that file -->

# caladaptR <img src="man/figures/caladaptr-beta_logo.svg" align="right" width="240" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

`caladaptr` is an API client that makes it easier to work with data from
[Cal-Adapt.org](https://cal-adapt.org/) in R.

‘Beta’ status means the package:

1)  is still under development  
2)  is being updated more often than usual  
3)  has a slightly higher probability than average that updates will
    *not* be backward compatible  
4)  is actively seeking user feedback and input (interested? join the
    [caladaptR betaR](#caladaptr-betar-club) club\!)

## Installation

`caladaptr` is hosted on
[GitHub](https://github.com/UCANR-IGIS/caladaptr). To install it, you
need to have [RTools](https://cran.r-project.org/bin/windows/Rtools/)
and the `devtools` package installed:

``` r
library(devtools)
devtools::install_github("ucanr-igis/caladaptr")
```

## Example 1: Get Annual Projected Temperature at a Point Location

1)  Load the package:

<!-- end list -->

``` r
library(caladaptr)
#> Cal-Adapt R (version 0.2.5)
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

**Note \#1**. values are returned with units, thanks to the
[units](https://cran.r-project.org/package=units) package. No more
guessing what the units are, or having to lookup unit conversion
constants\!

**Note \#2**. See how the results from `ca_getvals` get fed into
`ca_vals2tbl` to become a tibble? See `?ca_vals2tbl` for more options to
specify which column(s) to return.

## Example \#2: Same query as \#1, but for a Congressional District

Cal-Adapt supports \~10 ‘preset’ Areas of Interest. `caladaptr` refers
to these as "AOI Presets’, and you can use them to specify area(s) of
interest in your API requests:

``` r
aoipreset_types
#>  [1] "censustracts"      "counties"          "cdistricts"       
#>  [4] "ccc4aregions"      "climregions"       "hydrounits"       
#>  [7] "irwm"              "electricutilities" "wecc-load-area"   
#> [10] "evtlocations"      "place"
```

Next, we’ll modify the first API call by simply “swapping out” the point
location for a Congressional District location, using the
`ca_loc_aoipreset()` function. For this example we’ll use California’s
20th and 24th [Congressional
Districts](https://en.wikipedia.org/wiki/California%27s_congressional_districts)
(Carmel Valley and Santa Barbara).

``` r
cap2 <- cap1 %>% ca_loc_aoipreset(type = "cdistricts",
                                  idfld = "geoid",
                                  idval = c("0620", "0624"),
                                  stat = "mean")

cap2
#> Cal-Adapt API Request
#> Location(s): 
#>   AOI Preset: cdistricts
#>   geoid(s): 0620, 0624
#>   Stat(s): mean
#> Variable(s): tasmax, tasmin
#> Temporal aggregration period(s): year
#> GCM(s): HadGEM2-ES, CNRM-CM5, CanESM2, MIROC5
#> Scenario(s): rcp45, rcp85
#> Dates: 2040-01-01 to 2060-01-01
#> Options: NA
```

Note when you query a polygon, you have to also specify a summary
statistic that will be used to reduce the various pixels that fall
within the polygon to one number. Your choices are `mean`, `max`,
`median`, `min`, and `sum.` (If you want to retain the individual pixel
values, use `ca_getrst()`.)

``` r
cap2_vals_df <- ca_getvals(cap2, quiet = TRUE) %>% ca_vals2tbl()

dim(cap2_vals_df)
#> [1] 672   7
head(cap2_vals_df)
#> # A tibble: 6 x 7
#>   feat_id    cvar   period gcm        scenario dt              val
#>   <fct>      <fct>  <fct>  <fct>      <fct>    <chr>           [K]
#> 1 geoid_0620 tasmax year   HadGEM2-ES rcp45    2040-01-01 295.7161
#> 2 geoid_0620 tasmax year   HadGEM2-ES rcp45    2041-01-01 296.1374
#> 3 geoid_0620 tasmax year   HadGEM2-ES rcp45    2042-01-01 295.5295
#> 4 geoid_0620 tasmax year   HadGEM2-ES rcp45    2043-01-01 297.1063
#> 5 geoid_0620 tasmax year   HadGEM2-ES rcp45    2044-01-01 297.3475
#> 6 geoid_0620 tasmax year   HadGEM2-ES rcp45    2045-01-01 295.8374
```

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

# Coming soon…

  - How to query a `sf` object  
  - More examples of reshaping data  
  - Retrieving rasters
