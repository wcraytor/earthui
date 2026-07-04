# test-fit_gam_extra.R — Additional fit_gam tests

test_that("fit_gam result has expected class", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  expect_s3_class(res, "mgcvUI_result")
  expect_true("model" %in% names(res))
  expect_true("formula" %in% names(res))
  expect_true("response" %in% names(res))
  expect_true("smooth_specs" %in% names(res))
  expect_true("elapsed" %in% names(res))
})

test_that("fit_gam with GCV method", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, method = "GCV.Cp", cv_folds = 0)
  expect_s3_class(res$model, "gam")
  expect_true(grepl("GCV", res$model$method))
})

test_that("fit_gam with gamma > 1", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, gamma = 1.4, cv_folds = 0)
  expect_s3_class(res$model, "gam")
})

test_that("fit_gam with select = TRUE", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "hp", type = "s", bs = "tp", k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, select = TRUE, cv_folds = 0)
  expect_s3_class(res$model, "gam")
})

test_that("fit_gam subsets to relevant columns only", {
  # Add a column with all NAs -- should NOT affect the fit
  df <- mtcars
  df$all_na <- NA_real_
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(df, "mpg", specs, cv_folds = 0)
  expect_s3_class(res$model, "gam")
  # Should use all 32 rows since all_na is excluded
  expect_equal(nrow(res$model$model), 32L)
})

test_that("fit_gam with NAs in model columns drops rows", {
  df <- mtcars
  df$wt[1:3] <- NA
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(df, "mpg", specs, cv_folds = 0)
  expect_s3_class(res$model, "gam")
  expect_equal(nrow(res$model$model), 29L)
})

test_that("fit_gam preserves response name", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  expect_equal(res$response, "mpg")
})

test_that("fit_gam preserves smooth specs", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "cyl", type = "linear", bs = NULL, k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  expect_true(length(res$smooth_specs) >= 1L)
})

test_that("fit_gam elapsed time is non-negative", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  expect_true(res$elapsed >= 0)
})

test_that("fit_gam with Gamma family", {
  df <- mtcars
  df$mpg_pos <- df$mpg  # already positive
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(df, "mpg_pos", specs,
                 family = Gamma(link = "log"), cv_folds = 0)
  expect_s3_class(res$model, "gam")
})

test_that("fit_gam with multiple predictors produces reasonable R-squared", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "hp", type = "s", bs = "tp", k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  r2 <- summary(res$model)$r.sq
  expect_true(r2 > 0.5)  # wt+hp should explain mpg well
})
