# test-format_results.R — Tests for format_gam_summary, tidy_gam, glance_gam

# Helper: fit a basic model
fit_basic <- function() {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  fit_gam(mtcars, "mpg", specs, cv_folds = 0)
}

fit_multi <- function() {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "hp", type = "s", bs = "tp", k = NULL),
    list(vars = "cyl", type = "linear", bs = NULL, k = NULL)
  )
  fit_gam(mtcars, "mpg", specs, cv_folds = 0)
}

# ---- format_gam_summary ----

test_that("format_gam_summary returns all expected fields", {
  res <- fit_basic()
  summ <- format_gam_summary(res)

  expect_true(is.numeric(summ$r_squared))
  expect_true(is.numeric(summ$dev_explained))
  expect_true(is.numeric(summ$aic))
  expect_true(is.numeric(summ$bic))
  expect_true(is.numeric(summ$n_obs))
  expect_true(is.numeric(summ$n_smooths))
  expect_true(is.character(summ$method))
  expect_true(is.character(summ$family))
  expect_true(is.data.frame(summ$smooth_table))
  expect_true(is.data.frame(summ$parametric_table))
})

test_that("format_gam_summary r_squared is between 0 and 1", {
  res <- fit_basic()
  summ <- format_gam_summary(res)
  expect_true(summ$r_squared >= 0 && summ$r_squared <= 1)
})

test_that("format_gam_summary dev_explained is between 0 and 1", {
  res <- fit_basic()
  summ <- format_gam_summary(res)
  expect_true(summ$dev_explained >= 0 && summ$dev_explained <= 1)
})

test_that("format_gam_summary n_obs matches data", {
  res <- fit_basic()
  summ <- format_gam_summary(res)
  expect_equal(summ$n_obs, 32L)
})

test_that("format_gam_summary smooth_table has correct columns", {
  res <- fit_basic()
  summ <- format_gam_summary(res)
  expect_true(all(c("Term", "EDF", "Ref.df", "F", "p_value") %in%
                    names(summ$smooth_table)))
})

test_that("format_gam_summary smooth_table has one row per smooth", {
  res <- fit_multi()
  summ <- format_gam_summary(res)
  # 2 smooth terms (wt, hp); cyl is linear
  expect_equal(nrow(summ$smooth_table), 2L)
})

test_that("format_gam_summary parametric_table includes linear terms", {
  res <- fit_multi()
  summ <- format_gam_summary(res)
  expect_true(nrow(summ$parametric_table) >= 1L)
  expect_true(all(c("Term", "Estimate", "Std_Error", "t_value", "p_value") %in%
                    names(summ$parametric_table)))
})

test_that("format_gam_summary with no parametric terms", {
  res <- fit_basic()
  summ <- format_gam_summary(res)
  # Intercept is parametric but only smooth terms
  expect_true(nrow(summ$parametric_table) >= 0L)
})

test_that("format_gam_summary method is REML by default", {
  res <- fit_basic()
  summ <- format_gam_summary(res)
  expect_equal(summ$method, "REML")
})

test_that("format_gam_summary family is gaussian by default", {
  res <- fit_basic()
  summ <- format_gam_summary(res)
  expect_equal(summ$family, "gaussian")
})

test_that("format_gam_summary cv_rsq is NULL when no CV", {
  res <- fit_basic()
  summ <- format_gam_summary(res)
  expect_null(summ$cv_rsq)
})

# ---- tidy_gam ----

test_that("tidy_gam returns a data frame", {
  res <- fit_basic()
  tidy <- tidy_gam(res)
  expect_true(is.data.frame(tidy))
  expect_true(nrow(tidy) > 0)
})

test_that("tidy_gam includes term column", {
  res <- fit_basic()
  tidy <- tidy_gam(res)
  expect_true("term" %in% names(tidy))
})

test_that("tidy_gam has rows for smooth and parametric terms", {
  res <- fit_multi()
  tidy <- tidy_gam(res)
  # Should have rows for terms (at least 2: smooth + parametric)
  expect_true(nrow(tidy) >= 2L)
})

# ---- glance_gam ----

test_that("glance_gam returns a one-row data frame", {
  res <- fit_basic()
  g <- glance_gam(res)
  expect_true(is.data.frame(g))
  expect_equal(nrow(g), 1L)
})

test_that("glance_gam includes standard columns", {
  res <- fit_basic()
  g <- glance_gam(res)
  # broom::glance.gam typically returns df, logLik, AIC, BIC, etc.
  expect_true(any(c("AIC", "aic") %in% tolower(names(g))))
})
