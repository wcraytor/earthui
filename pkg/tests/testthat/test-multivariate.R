# --- Multi-response fit_earth() ---

test_that("fit_earth fits a multi-response model", {
  result <- fit_earth(mtcars, target = c("mpg", "hp"),
                      predictors = c("cyl", "disp", "wt"))
  expect_s3_class(result, "earthui_result")
  expect_equal(result$target, c("mpg", "hp"))
  expect_equal(result$predictors, c("cyl", "disp", "wt"))
  expect_false(result$cv_enabled)

  # Model should be a valid earth object with multi-response
  expect_true(!is.null(result$model))
  # Coefficients matrix should have one column per response
  coef_mat <- result$model$coefficients
  expect_equal(ncol(coef_mat), 2L)
  expect_true(all(c("mpg", "hp") %in% colnames(coef_mat)))
})

test_that("fit_earth multi-response builds cbind formula", {
  result <- fit_earth(mtcars, target = c("mpg", "hp"),
                      predictors = c("cyl", "wt"))
  # Fitted values should have 2 columns
  fitted_mat <- stats::fitted(result$model)
  expect_equal(ncol(fitted_mat), 2L)
  # Residuals should have 2 columns
  resid_mat <- stats::residuals(result$model)
  expect_equal(ncol(resid_mat), 2L)
})

test_that("fit_earth multi-response with CV", {
  result <- fit_earth(mtcars, target = c("mpg", "hp"),
                      predictors = c("cyl", "wt"),
                      degree = 2L)
  expect_true(result$cv_enabled)
  expect_equal(result$degree, 2L)
})

test_that("fit_earth multi-response validates missing targets", {
  expect_error(
    fit_earth(mtcars, target = c("mpg", "nonexistent"),
              predictors = c("cyl", "wt")),
    "not found"
  )
})

test_that("fit_earth multi-response validates target/predictor overlap", {
  expect_error(
    fit_earth(mtcars, target = c("mpg", "hp"),
              predictors = c("cyl", "hp", "wt")),
    "must not be in"
  )
})

test_that("fit_earth multi-response handles categoricals", {
  df <- mtcars
  df$am_cat <- as.character(df$am)
  result <- fit_earth(df, target = c("mpg", "hp"),
                      predictors = c("wt", "am_cat"),
                      categoricals = "am_cat")
  expect_s3_class(result, "earthui_result")
  expect_equal(result$categoricals, "am_cat")
})

test_that("fit_earth multi-response handles missing values", {
  df <- mtcars
  df$wt[1:3] <- NA
  expect_message(
    result <- fit_earth(df, c("mpg", "hp"), c("wt", "cyl")),
    "Removed 3 rows"
  )
  expect_s3_class(result, "earthui_result")
  expect_equal(nrow(result$data), 29L)
})

# --- Multi-response format_summary() ---

test_that("format_summary returns per-response metrics for multi-response", {
  result <- fit_earth(mtcars, c("mpg", "hp"), c("cyl", "disp", "wt"))
  s <- format_summary(result)

  expect_true(isTRUE(s$multi))

  # R-squared should be a named vector of length 2

  expect_length(s$r_squared, 2L)
  expect_equal(names(s$r_squared), c("mpg", "hp"))
  expect_true(all(s$r_squared >= 0 & s$r_squared <= 1))

  # GRSq, GCV, RSS should also be per-response
  expect_length(s$grsq, 2L)
  expect_equal(names(s$grsq), c("mpg", "hp"))

  expect_length(s$gcv, 2L)
  expect_equal(names(s$gcv), c("mpg", "hp"))
  expect_true(all(s$gcv >= 0))

  expect_length(s$rss, 2L)
  expect_equal(names(s$rss), c("mpg", "hp"))
  expect_true(all(s$rss >= 0))
})

test_that("format_summary multi-response coefficients have per-target columns", {
  result <- fit_earth(mtcars, c("mpg", "hp"), c("cyl", "disp", "wt"))
  s <- format_summary(result)

  expect_s3_class(s$coefficients, "data.frame")
  expect_true("term" %in% names(s$coefficients))
  expect_true("mpg" %in% names(s$coefficients))
  expect_true("hp" %in% names(s$coefficients))
})

test_that("format_summary multi-response with CV returns per-response cv_rsq", {
  result <- fit_earth(mtcars, c("mpg", "hp"), c("cyl", "wt"), degree = 2L)
  s <- format_summary(result)

  expect_length(s$cv_rsq, 2L)
  expect_equal(names(s$cv_rsq), c("mpg", "hp"))
  expect_true(all(is.numeric(s$cv_rsq)))
})

test_that("format_summary single-response still returns scalars", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "wt"))
  s <- format_summary(result)

  expect_false(isTRUE(s$multi))
  expect_length(s$r_squared, 1L)
  expect_length(s$gcv, 1L)
  expect_length(s$grsq, 1L)
  expect_length(s$rss, 1L)
})

# --- Multi-response format_anova() ---

test_that("format_anova returns per-target coefficient columns for multi-response", {
  result <- fit_earth(mtcars, c("mpg", "hp"), c("cyl", "disp", "wt"))
  anova_df <- format_anova(result)

  expect_s3_class(anova_df, "data.frame")
  expect_true("term" %in% names(anova_df))
  expect_true("description" %in% names(anova_df))
  expect_true("mpg" %in% names(anova_df))
  expect_true("hp" %in% names(anova_df))
  expect_true(nrow(anova_df) > 0L)

  # Coefficients should be numeric
  expect_true(is.numeric(anova_df[["mpg"]]))
  expect_true(is.numeric(anova_df[["hp"]]))
})

test_that("format_anova single-response has target-named coefficient column", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "wt"))
  anova_df <- format_anova(result)

  expect_true("mpg" %in% names(anova_df))
  expect_false("coefficient" %in% names(anova_df))
})

# --- Multi-response format_model_equation() ---

test_that("format_model_equation returns earthui_equation_multi for multi-response", {
  result <- fit_earth(mtcars, c("mpg", "hp"), c("cyl", "disp", "wt"))
  eq <- format_model_equation(result)

  expect_s3_class(eq, "earthui_equation_multi")
  expect_true(eq$multi)
  expect_equal(eq$targets, c("mpg", "hp"))
  expect_length(eq$equations, 2L)
  expect_equal(names(eq$equations), c("mpg", "hp"))
})

test_that("format_model_equation multi-response per-equation has LaTeX fields", {
  result <- fit_earth(mtcars, c("mpg", "hp"), c("cyl", "disp", "wt"))
  eq <- format_model_equation(result)

  for (tgt in c("mpg", "hp")) {
    sub_eq <- eq$equations[[tgt]]
    expect_s3_class(sub_eq, "earthui_equation")
    expect_true(nzchar(sub_eq$latex))
    expect_true(nzchar(sub_eq$latex_inline))
    expect_true(nzchar(sub_eq$latex_pdf))
    expect_true(nzchar(sub_eq$latex_word))
    expect_true(is.list(sub_eq$groups))
    expect_true(length(sub_eq$groups) > 0L)
  }
})

test_that("format_model_equation with specific response_idx returns single equation", {
  result <- fit_earth(mtcars, c("mpg", "hp"), c("cyl", "disp", "wt"))

  eq1 <- format_model_equation(result, response_idx = 1L)
  expect_s3_class(eq1, "earthui_equation")
  expect_true(nzchar(eq1$latex))

  eq2 <- format_model_equation(result, response_idx = 2L)
  expect_s3_class(eq2, "earthui_equation")
  expect_true(nzchar(eq2$latex))

  # Equations should differ (different coefficients)
  expect_false(identical(eq1$latex, eq2$latex))
})

test_that("format_model_equation single-response returns earthui_equation class", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "wt"))
  eq <- format_model_equation(result)

  expect_s3_class(eq, "earthui_equation")
  expect_true(nzchar(eq$latex))
  expect_true(nzchar(eq$latex_inline))
  expect_true(grepl("\\\\begin\\{array\\}", eq$latex))
  expect_true(is.list(eq$groups))
})

# --- Multi-response plots ---

test_that("plot_residuals works with multi-response and response_idx", {
  result <- fit_earth(mtcars, c("mpg", "hp"), c("cyl", "disp", "wt"))

  p1 <- plot_residuals(result, response_idx = 1L)
  expect_s3_class(p1, "ggplot")
  expect_true(grepl("mpg", p1$labels$title))

  p2 <- plot_residuals(result, response_idx = 2L)
  expect_s3_class(p2, "ggplot")
  expect_true(grepl("hp", p2$labels$title))
})

test_that("plot_residuals multi-response defaults to first response", {
  result <- fit_earth(mtcars, c("mpg", "hp"), c("cyl", "disp", "wt"))
  p <- plot_residuals(result)
  expect_s3_class(p, "ggplot")
  expect_true(grepl("mpg", p$labels$title))
})

test_that("plot_qq works with multi-response and response_idx", {
  result <- fit_earth(mtcars, c("mpg", "hp"), c("cyl", "disp", "wt"))

  p1 <- plot_qq(result, response_idx = 1L)
  expect_s3_class(p1, "ggplot")
  expect_true(grepl("mpg", p1$labels$title))

  p2 <- plot_qq(result, response_idx = 2L)
  expect_s3_class(p2, "ggplot")
  expect_true(grepl("hp", p2$labels$title))
})

test_that("plot_actual_vs_predicted works with multi-response", {
  result <- fit_earth(mtcars, c("mpg", "hp"), c("cyl", "disp", "wt"))

  p1 <- plot_actual_vs_predicted(result, response_idx = 1L)
  expect_s3_class(p1, "ggplot")
  expect_true(grepl("mpg", p1$labels$title))

  p2 <- plot_actual_vs_predicted(result, response_idx = 2L)
  expect_s3_class(p2, "ggplot")
  expect_true(grepl("hp", p2$labels$title))
})

test_that("plot_partial_dependence works with multi-response", {
  result <- fit_earth(mtcars, c("mpg", "hp"), c("cyl", "disp", "wt"))

  p1 <- plot_partial_dependence(result, "wt", response_idx = 1L)
  expect_s3_class(p1, "ggplot")
  expect_true(grepl("mpg", p1$labels$y))

  p2 <- plot_partial_dependence(result, "wt", response_idx = 2L)
  expect_s3_class(p2, "ggplot")
  expect_true(grepl("hp", p2$labels$y))
})

test_that("plot_contribution works with multi-response", {
  result <- fit_earth(mtcars, c("mpg", "hp"), c("cyl", "disp", "wt"))

  p1 <- plot_contribution(result, "wt", response_idx = 1L)
  expect_s3_class(p1, "ggplot")
  expect_true(grepl("mpg", p1$labels$y))

  p2 <- plot_contribution(result, "wt", response_idx = 2L)
  expect_s3_class(p2, "ggplot")
  expect_true(grepl("hp", p2$labels$y))
})

test_that("plot_correlation_matrix handles multi-response", {
  result <- fit_earth(mtcars, c("mpg", "hp"), c("cyl", "disp", "wt"))
  p <- plot_correlation_matrix(result)
  expect_s3_class(p, "ggplot")
})

test_that("list_g_functions works with multi-response", {
  result <- fit_earth(mtcars, c("mpg", "hp"), c("cyl", "disp", "wt"))
  gf <- list_g_functions(result)
  expect_s3_class(gf, "data.frame")
  expect_true(nrow(gf) > 0L)
  expect_true("label" %in% names(gf))
})

test_that("plot_g_function works with multi-response", {
  result <- fit_earth(mtcars, c("mpg", "hp"), c("cyl", "disp", "wt"))
  gf <- list_g_functions(result)
  if (nrow(gf) > 0L) {
    p <- plot_g_function(result, 1L, response_idx = 1L)
    expect_s3_class(p, "ggplot")

    p2 <- plot_g_function(result, 1L, response_idx = 2L)
    expect_s3_class(p2, "ggplot")
  }
})

# --- Weights parameter ---

test_that("fit_earth passes weights to earth", {
  w <- runif(nrow(mtcars), 0.5, 1.5)
  result <- fit_earth(mtcars, "mpg", c("cyl", "wt"), weights = w)
  expect_s3_class(result, "earthui_result")
  # Model should have fitted successfully
  expect_true(!is.null(result$model))
})

test_that("fit_earth passes weights with multi-response", {
  w <- runif(nrow(mtcars), 0.5, 1.5)
  result <- fit_earth(mtcars, c("mpg", "hp"), c("cyl", "wt"), weights = w)
  expect_s3_class(result, "earthui_result")
  expect_equal(ncol(result$model$coefficients), 2L)
})
