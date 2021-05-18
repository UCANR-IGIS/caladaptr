## Test functions that fetch data
## We wrap these in with_mock_api which

## testthat has a function skip_on_cran()
## that you can use to not run tests on CRAN. We recommend using this on all functions that are
## API calls since they are quite likely to fail on CRAN. These tests will still run on Travis.


with_mock_api({

  ##########################################################
  test_that("Fetching data for a single point", {
    pt1_cap <- ca_example_apireq(1)
    pt1_vals <- pt1_cap %>% ca_getvals_tbl()

    expect_equal(nrow(pt1_vals), 84)
    expect_equal(sum(as.numeric(pt1_vals$val)), 25110.4350, tolerance = .001)
    expect_s3_class(pt1_vals$val, "units")
  })

  #####################################################################################
  ## Second fetch test - 3 congressional districts with monthly date from a slug for 4 years
  #####################################################################################

  test_that("Fetching data for three congressional districts (AOI Preset)", {
    aoi_baseflow_cap <- ca_example_apireq(2)
    aoi_baseflow_tbl <- aoi_baseflow_cap %>% ca_getvals_tbl()

    expect_equal(nrow(aoi_baseflow_tbl), 144)
    expect_equal(ncol(aoi_baseflow_tbl), 5)
    expect_equal(min(aoi_baseflow_tbl$dt), "2080-01-31")
    expect_equal(sum(as.numeric(aoi_baseflow_tbl$val)), 21.3644, tolerance = 0.001)
  })

  #####################################################################################
  ## Third fetch test - sf object one feature, 1 GCM, 1 scenario, 3 years of daily data
  #####################################################################################

  ## this works but only after I manually change the json file created by capture_request()
  ## need to keep investigating, might have something to do with the name of the temporary geojson file
  ## or a random string that is inserted as part of the POST request
  ## Consider pulling this one out, and telling it to not use httptest caching.

  if (FALSE) {
  test_that("Fetching data for a one-feature sf polygon", {

    simp_poly_cap <- ca_example_apireq(3)
    simp_poly_tbl <- ca_getvals_tbl(simp_poly_cap)

    expect_equal(nrow(simp_poly_tbl), 1096)
    expect_s3_class(simp_poly_tbl, "tbl_df")
    expect_equal(as.numeric(sum(simp_poly_tbl$val)), 0.04269988, tolerance = 0.0001)

  })
  }

  ###############################################################
  ## Fourth fetch test - sf data frame with two polygons,
  ## 1 GCM, 1 RCP, 30 yrs of annual data
  ###############################################################
  if (FALSE) {
  test_that("Fetching data for a two-feature sf polygon", {

    cnty2_cap <- ca_example_apireq(4)
    cnty2_cap_tbl <- ca_getvals_tbl(cnty2_cap)
    expect_equal(nrow(cnty2_cap_tbl), 42)
    expect_equal(ncol(cnty2_cap_tbl), 8)
    expect_equal(as.numeric(sum(cnty2_cap_tbl$val)), 11723.3056, tolerance = 0.0001)

  })
  }


})

## Testing fetching data with a custom sf object can not be cached with
## httptest, because every time you run it it generates a new geojson file
## for the body of the POST request. Hence this has to be done outside
## with_mock_api.

# if (curl::has_internet()) {
#
#
# }


