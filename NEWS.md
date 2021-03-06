# caladaptr 0.4.6 (2021-01-12)

* `ca_db_info()`: returned result is no longer invisible
* `ca_getvals_tbl()`: `shiny_progress` argument added to show a progress bar in Shiny apps
* `ca_getvals_db()`: added `write_sidecar` argument - writes a sidecar file next to the SQLite file
* `ca_read_db()`: imports a SQLite file created by `ca_getvals_db()`, returning a remote tibble
* `shiny` added to imports

# caladaptr 0.4.5 (2021-01-02)

The highlight of this update is improvements to `ca_getrst_stars()`. You can now download cropped rasters for user-defined sf data frames as well, points, and preset areas of interest. Rasters can also be masked to the polygon boundary.

* `ca_getrst_stars()`: added support sf data frames and point locations; added `mask` and `merge_geom` arguments
* `geojsonsf` and `zip` added to imports
* `ca_apicalls()`: fixed a bug in the api_url when `ignore_spag = TRUE`

# caladaptr 0.4.4 (2020-12-07)

* `ca_example_apireq()`: updated example #1
* `ca_locagrid_geom()`: zip file download source changed to github.com/ucanr-igis/caladaptr-res/... 
* `ca_aoipreset_geom()`: zip file download source changed to github.com/ucanr-igis/caladaptr-res/... 
* `ca_getrst_stars()`: added
* `ca_read_stars()`: added

# caladaptr 0.4.3 (2020-12-06)

* `ca_getvals_tbl()`, `ca_getvals_db()` and `ca_apicalls()`: removed format=json parameter in favor of accept_json()
* `ca_resp_check()`: deleted (functionality absorbed into `ca_getvals_tbl()` & `ca_getvals_db()`)
* testhttp added to suggests; tests added
* `ca_lof_sf()`: trap added for multipoint features (which the Cal-Adapt server treats as individual points, hence they should not be used)
* `ca_apicalls()`: removed gson_fn_base from the list object returned
* `ca_example_apireq()`: added
* `ca_locagrid_geom()`: added
* `ca_aoipreset_geom()`: temporary zip file(s) are now deleted when no longer needed; source directory for zipfiles on GitHub changed from 'aoipreset_geoms' to just 'geoms'; check for internet connection added
* `plot.ca_apireq()`: `locagrid` argument added to overlay the loca grid 

# caladaptr 0.4.2 (2020-11-18)

* `plot.ca_apireq()`: default symbol size tweaked for point features

# caladaptr 0.4.1 (2020-11-10)

* `ca_getvals()` split into `ca_getvals_tbl()` and `ca_getvals_db()`; user_agent added to headers; support for sf objects added
* `ca_lof_sf()`: overhauled
* `ca_apicalls()`: new method for hashing API calls implemented; now returns a list object
* `aoipreset_idvals`: `name` removed as a field for uniquely identifying counties 
* `ca_loc_sf()` overhauled
* package `digest` added to imports

# caladaptr 0.4.0 (2020-10-11)

* `ca_slug()` added   
* `ca_getvals()` completely overhauled - returns a tibble
* `ca_apicalls()` added
* `ca_vals2tbl()` deleted (no longer needed)   
* `aoipreset_idflds`: `name` removed as a field for uniquely identifying counties 
* `plot.ca_apireq()`: added `static` argument
* packages `curl`, `RSQLite`, `dbplyr`, and `fastmatch` added to imports (i.e., required)

# caladaptr 0.3.0 (2020-09-18)

* updated read.csv calls in `ca_catalog_rs()`  for consistent behavior between R3.x and R4.x.   
* updated `ca_getvals()` to i) handle aoipreset locations where `idval = NULL`, and ii) display a progress bar for each location queried

# caladaptr 0.2.9 (2020-08-31)

* Fixed a capitalization bug in `ca_loc_pt()`  
* Expanding function help in numerous functions

# caladaptr 0.2.8 (2020-08-29)

* Added argument `force_day_one` to `ca_vals2tbl()` 
* Added a `NEWS.md` file to track changes to the package.

# caladaptr 0.2.7 (2020-08-28)

* Numerous little updates
* Added R notebook caladaptr_intro.Rmd

# caladaptr 0.2.6 (2020-08-27)

* Numerous updates

# caladaptr 0.2.5 (2020-08-21)

* Numerous updates

# caladaptr 0.2.4 (2020-08-12)

* Numerous updates

# caladaptr 0.2.3 (2020-08-11)

* Numerous updates

# caladaptr 0.2.2 (2020-08-09)

* Initial release

