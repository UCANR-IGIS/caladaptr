test_that("ca_catalog_search works correctly", {
  res_tbl <- ca_catalog_search("pr_day_gridmet", quiet = TRUE)
  expect_equal(nrow(res_tbl), 1)
  expect_equal(ncol(res_tbl), 13)
  expect_equal(res_tbl$id, 338)
})

test_that("Raster Series Catalog works", {
  catalog_df <- ca_catalog_rs(quiet = TRUE)
  expect_s3_class(catalog_df, "data.frame")
  expect_equal(length(catalog_df), 13)
  expect_equal(catalog_df %>% filter(slug == "tasmax_30yavg_ens10_rcp45") %>% pull(url),
               "https://api.cal-adapt.org/api/series/tasmax_30yavg_ens10_rcp45/")
})


