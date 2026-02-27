result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))

test_that("plot_variable_importance returns a ggplot", {
  p <- plot_variable_importance(result)
  expect_s3_class(p, "ggplot")
})

test_that("plot_variable_importance handles type argument", {
  p <- plot_variable_importance(result, type = "gcv")
  expect_s3_class(p, "ggplot")
})

test_that("plot_partial_dependence returns a ggplot", {
  p <- plot_partial_dependence(result, "wt")
  expect_s3_class(p, "ggplot")
})

test_that("plot_partial_dependence rejects missing variable", {
  expect_error(plot_partial_dependence(result, "nonexistent"), "not found")
})

test_that("plot_residuals returns a ggplot", {
  p <- plot_residuals(result)
  expect_s3_class(p, "ggplot")
})

test_that("plot_qq returns a ggplot", {
  p <- plot_qq(result)
  expect_s3_class(p, "ggplot")
})

test_that("plot_actual_vs_predicted returns a ggplot", {
  p <- plot_actual_vs_predicted(result)
  expect_s3_class(p, "ggplot")
})

test_that("plot functions reject non-earthui_result objects", {
  expect_error(plot_variable_importance(list()), "earthui_result")
  expect_error(plot_residuals(list()), "earthui_result")
  expect_error(plot_qq(list()), "earthui_result")
  expect_error(plot_actual_vs_predicted(list()), "earthui_result")
})

test_that("plot_partial_dependence works with categorical variable", {
  df <- mtcars
  result_cat <- fit_earth(df, "mpg", c("cyl", "hp", "wt"),
                          categoricals = "cyl")
  p <- plot_partial_dependence(result_cat, "cyl")
  expect_s3_class(p, "ggplot")
})
