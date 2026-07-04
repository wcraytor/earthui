test_that("plot_smooths returns a ggplot", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  p <- plot_smooths(res)
  expect_true(inherits(p, "gg") || inherits(p, "ggplot") ||
                inherits(p, "patchwork"))
})

test_that("plot_smooth_single returns a ggplot", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  p <- plot_smooth_single(res, "wt")
  expect_s3_class(p, "gg")
})

test_that("plot_smooth_single errors for unknown variable", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  expect_error(plot_smooth_single(res, "nonexistent"), "No smooth term")
})

test_that("plot_smooth_interactive returns a plotly object", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  p <- plot_smooth_interactive(res, "wt")
  expect_s3_class(p, "plotly")
})

test_that("plot_diagnostics returns a ggplot", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  p <- plot_diagnostics(res)
  expect_true(inherits(p, "gg") || inherits(p, "ggplot") ||
                inherits(p, "patchwork"))
})

test_that("plot_actual_vs_predicted_mgcv returns a ggplot", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  p <- plot_actual_vs_predicted_mgcv(res)
  expect_s3_class(p, "gg")
})
