test_that("fit_gam converts Date columns to numeric", {
  df <- data.frame(
    y = rnorm(50),
    dt = as.Date("2024-01-01") + seq_len(50)
  )
  specs <- list(list(vars = "dt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(df, "y", specs, cv_folds = 0)

  expect_s3_class(res$model, "gam")
})

test_that("fit_gam converts POSIXct columns to numeric", {
  df <- data.frame(
    y = rnorm(50),
    ts = as.POSIXct("2024-01-01 00:00:00") + seq_len(50) * 3600
  )
  specs <- list(list(vars = "ts", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(df, "y", specs, cv_folds = 0)

  expect_s3_class(res$model, "gam")
})

test_that("fit_gam converts character linear terms to factor", {
  df <- mtcars
  df$cyl_chr <- as.character(df$cyl)
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "cyl_chr", type = "linear", bs = NULL, k = NULL)
  )
  res <- fit_gam(df, "mpg", specs, cv_folds = 0)

  expect_s3_class(res$model, "gam")
})

test_that("fit_gam handles NULL parameters defensively", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs,
                 family = NULL, method = NULL,
                 select = NULL, gamma = NULL, cv_folds = 0)

  expect_s3_class(res$model, "gam")
  expect_equal(res$model$method, "REML")
})

test_that("fit_gam validates missing variables", {
  specs <- list(list(vars = "nonexistent", type = "s", bs = "tp", k = NULL))
  expect_error(fit_gam(mtcars, "mpg", specs), "not in data")
})

test_that("fit_gam validates missing response", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  expect_error(fit_gam(mtcars, "nonexistent", specs), "not found in data")
})
