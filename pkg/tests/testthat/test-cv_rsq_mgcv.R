test_that("fit_gam computes CV R-squared when requested", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 5)

  expect_false(is.null(res$cv_rsq))
  expect_true(is.numeric(res$cv_rsq))
  expect_true(res$cv_rsq > 0 && res$cv_rsq < 1)
})

test_that("fit_gam skips CV when cv_folds = 0", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  expect_null(res$cv_rsq)
})

test_that("CV R-squared is less than in-sample R-squared", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "hp", type = "s", bs = "tp", k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 5)

  r2 <- summary(res$model)$r.sq
  # CV R² should generally be lower (less overfitting)
  # Allow some tolerance since it's stochastic
  expect_true(res$cv_rsq < r2 + 0.05)
})

test_that("format_gam_summary includes cv_rsq", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 5)
  summ <- format_gam_summary(res)

  expect_true("cv_rsq" %in% names(summ))
  expect_true(is.numeric(summ$cv_rsq))
})
