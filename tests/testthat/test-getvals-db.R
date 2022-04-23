# file.exists(pt1_db1_fn <- "D:\\GitHub\\cal-adapt\\caladaptr\\tests\\testthat\\test_data\\pt1_lkup-true.sqlite")

## pt1_db1_fn <- "./test_data/pt1_lkup-true.sqlite" THERE'S NO NEED TO PUT IT HERE SINCE WE CREATE AND DESTROY IT EACH TIME

pt1_db1_fn <- tempfile("~ca_pt1_", fileext = ".sqlite")

test_that("Fetching data for a single point into a SQLite database with lookup_tbls = TRUE", {
  skip_on_cran()
  skip_if_offline()
  pt1_cap <- ca_example_apireq(1)
  pt1_tbl <- pt1_cap %>% ca_getvals_db(db_fn = pt1_db1_fn, db_tbl = "tasmax", lookup_tbls = TRUE, quiet = TRUE)
  vals_per_gcm_tbl <- pt1_tbl %>% group_by(gcm) %>% count() %>% collect()   ## need to specify dplyr:: because these functions are not imported
  expect_equal(nrow(vals_per_gcm_tbl), 4)
  expect_equal(sum(vals_per_gcm_tbl$n), 84)
  expect_equal(class(vals_per_gcm_tbl$n), "integer")
  rm(pt1_tbl)
})

test_that("SQLite database is well formed", {
  skip_on_cran()
  skip_if_offline()
  db_conn <- dbConnect(SQLite(), pt1_db1_fn)
  db_info_lst <- DBI::dbGetInfo(db_conn)   ## have to preface DBI:: b/c dbGetInfo is not imported in the package (see NAMESPACE)
  expect_equal(basename(db_info_lst$dbname), basename(pt1_db1_fn))
  expect_equal(length(dbListTables(db_conn)), 7)
  cvars_tbl <- dbReadTable(db_conn, "cvars")
  expect_equal(nrow(cvars_tbl), 11)
  dbDisconnect(db_conn)
})

test_that("We can read the SQLite database back in with ca_db_read()", {
  skip_on_cran()
  skip_if_offline()
  pt1_imp_tbl <- ca_db_read(pt1_db1_fn)
  expect_equal(pt1_imp_tbl %>% pull(val) %>% sum(), 25110.44, tolerance = 0.01)
  pt1_imp_attr <- attributes(pt1_imp_tbl)
  expect_true("vals_tbl" %in% names(pt1_imp_attr))
  expect_true("lkp_sql" %in% names(pt1_imp_attr))
  expect_equal(attr(pt1_imp_tbl, "vals_tbl"), "tasmax")
  rm(pt1_imp_tbl)
})

test_that("ca_db_info() is working properly", {
  pt1_imp_tbl <- ca_db_read(pt1_db1_fn)
  pt1_imp_info <- ca_db_info(pt1_imp_tbl)
  expect_s3_class(pt1_imp_info, "ca_db_info")
  expect_length(pt1_imp_info, 3)
  expect_equal(class(pt1_imp_info$tbls), "list")
  expect_length(pt1_imp_info$tbls , 4)
  expect_equal(pt1_imp_info$tbls$val_tbl$tasmax$num_rows, 84)
  rm(pt1_imp_tbl)
})

unlink(pt1_db1_fn)

liv_tmp_fn <- tempfile("~ca_liv_test_download", fileext = ".sqlite")

test_that("We can save values to a brand new sqlite database", {
  skip_on_cran()
  skip_if_offline()
  liv_tmp1_rtbl <- ca_example_apireq(6) %>%
    ca_getvals_db(db_fn = liv_tmp_fn,
                  db_tbl = "my_vals",
                  lookup_tbls = TRUE,
                  new_recs_only = TRUE,
                  quiet = TRUE)
  expect_equal(ncol(liv_tmp1_rtbl), 7)
  expect_s3_class(liv_tmp1_rtbl, "tbl_dbi")
  expect_s3_class(liv_tmp1_rtbl, "tbl_SQLiteConnection")
  expect_setequal(c("src", "ops"), names(liv_tmp1_rtbl))
  ## LOOK FOR THE SIDECAR FILE .TXT
  rm(liv_tmp1_rtbl)
})

test_that("We can append values to a sqlite database", {
  skip_on_cran()
  skip_if_offline()
  liv_tmp2_rtbl <- ca_example_apireq(5) %>%
    ca_getvals_db(db_fn = liv_tmp_fn,
                  db_tbl = "my_vals",
                  lookup_tbls = TRUE,
                  new_recs_only = TRUE,
                  quiet = TRUE)
  expect_s3_class(liv_tmp2_rtbl, "tbl_dbi")
  expect_s3_class(liv_tmp2_rtbl, "tbl_SQLiteConnection")
  expect_equal(ncol(liv_tmp2_rtbl), 7)
  expect_equal(liv_tmp2_rtbl %>% count() %>% pull(n), 175320)
  rm(liv_tmp2_rtbl)
})

if (file.exists(liv_tmp_fn)) unlink(liv_tmp_fn)

test_that("Fetching db data for two Congressional Districts and a slug with no lookup tables", {
  skip_on_cran()
  skip_if_offline()
  cdist_bflow_fn <- tempfile("~ca_cdist_bflow_test_", fileext = ".sqlite")
  cdist_bflow_cap <- ca_example_apireq(2)
  cdist_bflow_rtbl <- cdist_bflow_cap %>% ca_getvals_db(db_fn = cdist_bflow_fn, db_tbl = "bflow", lookup_tbls = FALSE, quiet = TRUE)
  expect_s3_class(cdist_bflow_rtbl, "tbl_dbi")
  expect_s3_class(cdist_bflow_rtbl, "tbl_SQLiteConnection")
  expect_equal(ncol(cdist_bflow_rtbl), 5)
  expect_equal(cdist_bflow_rtbl %>% pull(val) %>% sum(), 21.3644, tolerance = 0.0001)
  rm(cdist_bflow_rtbl)
  if (file.exists(cdist_bflow_fn)) unlink(cdist_bflow_fn)
})

