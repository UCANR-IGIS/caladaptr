test_that("API requests are constructed properly", {
  expect_equal(length(ca_apireq()), 8)
  expect_equal(length(ca_gcm(gcm = NA)), 8)
  expect_equal(length(ca_scenario(scenario = NA)), 8)
  expect_equal(length(ca_dates(start = NA, end = NA)), 8)
  expect_equal(length(ca_years(start = NA, end = NA)), 8)
  expect_equal(length(ca_cvar(cvar = NA)), 8)
  expect_equal(length(ca_loc_pt(coords = c(-128.2, 38.5))), 8)
  expect_equal(length(ca_options()), 8)
  expect_equal(length(ca_slug(slug = NA)), 8)
  expect_equal(length(ca_period(period = NA)), 8)
})

test_that("Raster Series Catalog works", {
  catalog_df <- ca_catalog_rs(quiet = TRUE)
  expect_s3_class(catalog_df, "data.frame")
  expect_equal(length(catalog_df), 13)
  expect_equal(catalog_df %>% filter(slug == "tasmax_30yavg_ens10_rcp45") %>% pull(url),
               "https://api.cal-adapt.org/api/series/tasmax_30yavg_ens10_rcp45/")
})




