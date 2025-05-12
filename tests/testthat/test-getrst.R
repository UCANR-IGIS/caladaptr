test_that("ca_biggeom_blocks works on the CA state border", {
  skip_on_cran()
  skip_if_offline()
  ca_bnd_sf <- sf::st_read("https://raw.githubusercontent.com/ucanr-igis/caladaptr-res/main/geoms/ca_bnd.geojson", quiet = TRUE)
  ca_bnd_blocks_sf <- ca_biggeom_blocks(ca_bnd_sf, block_area_mi2 = 20000)
  expect_equal(nrow(ca_bnd_blocks_sf), 18)
})

test_that("ca_getrst_stars works", {
  skip_on_cran()
  skip_if_offline()
  simppoly_cap <- ca_example_apireq(3) %>% ca_options(spatial_ag = "none")
  simppoly_fn <- simppoly_cap %>% ca_getrst_stars(out_dir = tempdir(), quiet = TRUE)
  expect_equal(length(simppoly_fn), 1)
  expect_true(file.exists(simppoly_fn))
  simppoly_stars_lst <- ca_stars_read(simppoly_fn)
  expect_equal(as.numeric(dim(simppoly_stars_lst[[1]])), c(7,5,1096))
  unlink(simppoly_fn)
})

## Need a test for 6D merging

