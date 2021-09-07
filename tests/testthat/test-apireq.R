test_that("API requests are constructed properly", {
  expect_equal(length(ca_apireq()), 9)
  expect_equal(length(ca_gcm(gcm = NA)), 9)
  expect_equal(length(ca_scenario(scenario = NA)), 9)
  expect_equal(length(ca_dates(start = NA, end = NA)), 9)
  expect_equal(length(ca_years(start = NA, end = NA)), 9)
  expect_equal(length(ca_cvar(cvar = NA)), 9)
  expect_equal(length(ca_loc_pt(coords = c(-128.2, 38.5))), 9)
  expect_equal(length(ca_options()), 9)
  expect_equal(length(ca_slug(slug = NA)), 9)
  expect_equal(length(ca_period(period = NA)), 9)
})

test_that("Preflight check works", {
  cap1_good_yn <- ca_example_apireq(1) %>% ca_preflight(quiet=TRUE)
  expect_equal(cap1_good_yn, TRUE)
  cap6_good_yn <- ca_example_apireq(6) %>% ca_preflight(quiet=TRUE)
  expect_equal(cap6_good_yn, TRUE)
})

test_that("ca_apicalls works", {
  cap2 <- ca_example_apireq(2)
  cap2_apicalls_lst <- cap2 %>% ca_apicalls()
  expect_equal(nrow(cap2_apicalls_lst$api_tbl), 3)
  expect_equal(names(cap2_apicalls_lst$api_tbl)[1], "feat_id")
})

