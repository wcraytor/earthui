test_that("extract_smooth_grids returns named list of data frames", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "hp", type = "s", bs = "tp", k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  grids <- extract_smooth_grids(res, n_points = 20)

  expect_type(grids, "list")
  expect_true("wt" %in% names(grids))
  expect_true("hp" %in% names(grids))
  expect_equal(nrow(grids$wt), 20)
  expect_true(all(c("x", "fx") %in% names(grids$wt)))
})

test_that("extract_smooth_grids skips linear terms", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "cyl", type = "linear", bs = NULL, k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  grids <- extract_smooth_grids(res, n_points = 10)

  expect_true("wt" %in% names(grids))
  expect_false("cyl" %in% names(grids))
})

test_that("generate_r_code produces valid R code string", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  code <- generate_r_code(res, n_points = 10)

  expect_type(code, "character")
  expect_true(grepl("approxfun", code))
  expect_true(grepl("g_wt", code))
  expect_true(grepl("intercept", code))
})

test_that("generate_python_code produces valid Python code string", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  code <- generate_python_code(res, n_points = 10)

  expect_type(code, "character")
  expect_true(grepl("numpy", code))
  expect_true(grepl("g_wt", code))
  expect_true(grepl("INTERCEPT", code))
})

test_that("generate_cpp_code produces valid C++ code string", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  code <- generate_cpp_code(res, n_points = 10)

  expect_type(code, "character")
  expect_true(grepl("namespace gam_functions", code))
  expect_true(grepl("g_wt", code))
  expect_true(grepl("INTERCEPT", code))
})

test_that("export_functions_sqlite creates a valid database", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  tmp <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp))
  export_functions_sqlite(res, tmp, n_points = 10)

  expect_true(file.exists(tmp))
  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tables <- DBI::dbListTables(con)
  expect_true("smooth_grids" %in% tables)
  expect_true("model_info" %in% tables)
  expect_true("intercept" %in% tables)

  grids <- DBI::dbReadTable(con, "smooth_grids")
  expect_true(nrow(grids) > 0)
  expect_true("wt" %in% grids$variable)
})

test_that("export_functions_zip creates a zip file", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  tmp <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp))
  export_functions_zip(res, tmp, languages = c("R", "Python"), n_points = 10)

  expect_true(file.exists(tmp))
  contents <- utils::unzip(tmp, list = TRUE)$Name
  expect_true(any(grepl("gam_functions\\.R$", contents)))
  expect_true(any(grepl("gam_functions\\.py$", contents)))
})
