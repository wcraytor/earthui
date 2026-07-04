test_that("fit_gam caps k for low unique value variables", {
  # gear has 3 unique values — k should be capped
  specs <- list(list(vars = "gear", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  expect_s3_class(res$model, "gam")
})

test_that("fit_gam converts binary variables to factor", {
  df <- mtcars
  df$is_auto <- ifelse(df$am == 0, 1, 0)
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "is_auto", type = "s", bs = "tp", k = NULL)
  )

  res <- fit_gam(df, "mpg", specs, cv_folds = 0)
  expect_s3_class(res$model, "gam")
  # is_auto should have been forced to linear
  lin_specs <- Filter(function(s) s$type == "linear", res$smooth_specs)
  expect_true(length(lin_specs) >= 1)
})

test_that("fit_gam drops constant variables", {
  df <- mtcars
  df$const <- 42
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "const", type = "s", bs = "tp", k = NULL)
  )

  res <- fit_gam(df, "mpg", specs, cv_folds = 0)
  expect_s3_class(res$model, "gam")
  # const should have been dropped
  remaining_vars <- vapply(res$smooth_specs, function(s) s$vars, character(1))
  expect_false("const" %in% remaining_vars)
})

test_that("fit_gam errors when all predictors are dropped", {
  df <- mtcars
  df$const1 <- 1
  df$const2 <- 1
  specs <- list(
    list(vars = "const1", type = "s", bs = "tp", k = NULL),
    list(vars = "const2", type = "s", bs = "tp", k = NULL)
  )

  expect_error(fit_gam(df, "mpg", specs, cv_folds = 0),
               "No usable predictors")
})
