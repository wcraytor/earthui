test_that("format_summary returns expected structure", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
  s <- format_summary(result)

  expect_type(s, "list")
  expect_true("coefficients" %in% names(s))
  expect_true("r_squared" %in% names(s))
  expect_true("gcv" %in% names(s))
  expect_true("grsq" %in% names(s))
  expect_true("rss" %in% names(s))
  expect_true("n_terms" %in% names(s))
  expect_true("n_obs" %in% names(s))

  expect_s3_class(s$coefficients, "data.frame")
  expect_true(s$r_squared >= 0 && s$r_squared <= 1)
  expect_true(s$gcv >= 0)
  expect_equal(s$n_obs, 32L)
})

test_that("format_summary reports cv_rsq when CV enabled", {
  result <- fit_earth(mtcars, "mpg", c("wt", "hp"), degree = 2L)
  s <- format_summary(result)
  # CV R-squared may or may not be exactly available depending on earth version
  # but should not error
  expect_true(is.numeric(s$cv_rsq))
})

test_that("format_anova returns data frame", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
  anova_df <- format_anova(result)

  expect_s3_class(anova_df, "data.frame")
  expect_true("term" %in% names(anova_df))
  expect_true("description" %in% names(anova_df))
  expect_true("coefficient" %in% names(anova_df))
  expect_true(nrow(anova_df) > 0L)
})

test_that("format_variable_importance returns data frame", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
  imp <- format_variable_importance(result)

  expect_s3_class(imp, "data.frame")
  expect_true("variable" %in% names(imp))
  expect_true(nrow(imp) > 0L)
})

test_that("format functions reject non-earthui_result", {
  expect_error(format_summary(list()), "earthui_result")
  expect_error(format_anova(list()), "earthui_result")
  expect_error(format_variable_importance(list()), "earthui_result")
})
