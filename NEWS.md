# caladaptr 0.6.4 (2021-12-08)

* `ca_getvals_tbl()`: modified GET call to implement `timeout`

# caladaptr 0.6.3 (2021-12-07)

* `ca_getvals_tbl()`: added `timeout` argument to have the ability to increase the amount of time before httr returns a timeout error

# caladaptr 0.6.2 (2021-09-06)

* Added copyright holder and funding organization to the package authors 
* Updated package license from GPL-3 to GPL (>= 3)  
* logo updated - text and border are now green instead of red 

# caladaptr 0.6.1 (2021-09-06)

Version 0.6.1 is a fairly significant update, with several new and improved functions especially for downloading and working with rasters, SQLite databases, improved preflight testing for API requests, data catalog searching, customizing messages with a package-wide color scheme, and making function names more consistent. Also 5 vignettes on API Requests, Large Queries, and Rasters Part I, II, and III. 

* `caladaptr` now depends on R version 3.6.
* `ca_getrst_stars()`: modified to save additional attribute data in the sidecar files (e.g., gcm, scenario, climate variable, etc.); progress bar disabled if quiet = TRUE (e.g., for rmd); `normalize_path` argument added; `overwrite` argument added; `sidecar_write` argument renamed `sidecar`
* `ca_read_stars()`: deprecated (renamed `ca_stars_read()`)
* `ca_stars_read()`: replacement of `ca_read_stars()`, updated to accept a vector of TIF files names (instead of one), and return a list of stars objects; `read_sidecar` argument renamed `sidecar`; added `proxy` argument which imports the TIFs as stars proxy objects (i.e., disk pointers)
* `ca_stars_index`: new function to create an index for a list of stars rasters downloaded by `ca_getrst_stars` and bundled into a list with `ca_read_stars`
* `ca_stars_6d()`: combines stars rasters for different climate variables, GCMs, and emissions scenarios into a single 6-dimensional raster (with x, y, and date being the other three dimensions)
* `ca_biggeom_blocks()`: new function that takes a large geom and returns a simple feature data frame of rectangular blocks each one small enough to download TIFFs from the Cal-Adapt API 
* `ca_stars_mosaic()`: new function to mosaic stars rasters
* `ca_preflight()`: removed unused `quiet` argument; errors from ca_apicalls() now grouped and formatted
* `ca_settings()`: new function to customize package settings including the text output colors
* `ca_apicalls()`: `ignore_spag` renamed `spag_check`; new argument `check_for`; `preflight` renamed `pf`
* `ca_getvals_tbl()`: modified how ca_apicalls() is called, `omit_col` argument added
* `ca_getvals_db()`: updated to support Cal-Adapt API requests for Livneh data; `omit_col` argument added
* `ca_catalog_search()`: new function to search the raster series by slug and view properties
* `ca_catalog_rs()`: raster series data catalog updated (n=949)
* `ca_read_db()`: deprecated and renamed `ca_db_read()` for a more consistent API
* `ca_db_read()`: added `lkp_sql` and `vals_tbl` to the attributes of the result
* `ca_db_info()` and `format.ca_db_info()`: enhanced to read SQL statements from sidecar files, save and print multiple SQL statements
* `ca_catalog_fetch()`: now exported
* unit tests: new tests created for downloading rasters and saving data to SQLite; 68 in all
* five new vignettes: API Requests, Large Queries, Rasters Part I, II, and III

# caladaptr 0.5.0 (2021-05-17)

* `ca_catalog_fetch()`: trapped an error when tres property is missing
* `ca_apireq()`: added element for livneh
* `ca_livneh()`: new function to specify a Livneh dataset
* `cvars`: expanded to include more VIC variables
* `ca_apicalls()`: added additional error checks; support for livneh dataset
* `ca_preflight()`: new function to check an API request for errors

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

