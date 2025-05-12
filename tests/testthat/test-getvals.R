## Test functions that fetch data
## We wrap those functions that use GET calls in with_mock_api which uses cached results

## POST calls however are not compatible with httptest, because the post request generates - stay tuned, this might work now
## a ranodom value hence results can not be cached (I think)

with_mock_api({

  ## the mock_api works with the GET requests below but doesn't seem to work with POST requests.
  ## I manually change the json file created by capture_request()
  ## need to keep investigating, might have something to do with the name of the temporary geojson file
  ## or a random string that is inserted as part of the POST request
  ## Consider pulling this one out, and telling it to not use httptest caching.

  ##########################################################
  test_that("Fetching data for a single point", {
    skip_on_cran()
    skip_if_offline()

    pt1_cap <- ca_example_apireq(1)
    pt1_vals <- pt1_cap %>% ca_getvals_tbl(quiet = TRUE)
    expect_s3_class(pt1_vals, "tbl_df")
    expect_s3_class(pt1_vals$val, "units")
    expect_equal(nrow(pt1_vals), 84)
    expect_equal(sum(as.numeric(pt1_vals$val)), 25110.4350, tolerance = .001)
  })

  #####################################################################################
  ## Second fetch test - 3 congressional districts with monthly date from a slug for 4 years
  #####################################################################################

  test_that("Fetching data for three congressional districts (AOI Preset)", {
    skip_on_cran()
    skip_if_offline()

    aoi_baseflow_cap <- ca_example_apireq(2)
    aoi_baseflow_tbl <- aoi_baseflow_cap %>% ca_getvals_tbl(quiet = TRUE)

    expect_s3_class(aoi_baseflow_tbl, "tbl_df")
    expect_equal(nrow(aoi_baseflow_tbl), 144)
    expect_equal(ncol(aoi_baseflow_tbl), 5)
    expect_equal(min(aoi_baseflow_tbl$dt), "2080-01-31")
    expect_equal(sum(as.numeric(aoi_baseflow_tbl$val)), 21.3644, tolerance = 0.001)
  })


})   ## with mock API

#####################################################################################
## Fetch test - sf object one feature, Livneh data
## Fails inside mockAPI() (probably b/c it involves a POST request), so we put it outside.
#####################################################################################

test_that("Fetching data for five AOI presets (census tracts, daily tasmin/tasmax, Livneh", {
  skip_on_cran()
  skip_if_offline()

  liv_cap <- ca_example_apireq(6)
  liv_tbl <- liv_cap %>% ca_getvals_tbl(quiet = TRUE)
  expect_s3_class(liv_tbl, "tbl_df")
  expect_equal(nrow(liv_tbl), 21920)
  expect_equal(ncol(liv_tbl), 7)
  expect_equal(names(liv_tbl), c("tract", "cvar", "period", "slug", "spag", "dt", "val"))
  expect_equal(as.numeric(sum(liv_tbl$val)), 508522.5, tolerance = 0.01)
})

#####################################################################################
## Third fetch test - sf object one feature, 1 GCM, 1 scenario, 3 years of daily data
## Fails inside mockAPI() (probably b/c it involves a POST request), so we put it outside.
#####################################################################################

test_that("Fetching data for a one-feature sf polygon", {
  skip_on_cran()
  skip_if_offline()

  simp_poly_cap <- ca_example_apireq(3)
  simp_poly_tbl <- simp_poly_cap %>% ca_getvals_tbl(quiet = TRUE)
  expect_s3_class(simp_poly_tbl, "tbl_df")
  expect_equal(nrow(simp_poly_tbl), 1096)
  expect_s3_class(simp_poly_tbl, "tbl_df")
  expect_equal(as.numeric(sum(simp_poly_tbl$val)), 0.04269988, tolerance = 0.0001)
})

###############################################################
## Fetch test - sf data frame with two polygons,
## 1 GCM, 1 RCP, 30 yrs of annual data
## Can't use httptest because this involves a POST request
## This also fails within mockAPI(), so we put it outside.
###############################################################

test_that("Fetching data for a two-feature sf polygon", {
  skip_on_cran()
  skip_if_offline()

  cnty2_cap <- ca_example_apireq(4)
  cnty2_cap_tbl <- cnty2_cap %>% ca_getvals_tbl(quiet = TRUE)
  expect_s3_class(cnty2_cap_tbl, "tbl_df")
  expect_equal(nrow(cnty2_cap_tbl), 42)
  expect_equal(ncol(cnty2_cap_tbl), 8)
  expect_equal(as.numeric(sum(cnty2_cap_tbl$val)), 11723.3056, tolerance = 0.0001)
})


# capture_requests({
#   cnty2_cap <- ca_example_apireq(4)
#   cnty2_cap_tbl <- ca_getvals_tbl(cnty2_cap)
# })

