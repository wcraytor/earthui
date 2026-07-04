# test-fit_gam_advanced.R — Advanced fit_gam and formula building tests

# ---------------------------------------------------------------------------
# 1. Tensor product interactions: ti() terms
# ---------------------------------------------------------------------------

test_that("fit_gam with ti() tensor interaction term", {
  specs <- list(
    list(vars = "wt",  type = "s",  bs = "tp", k = NULL),
    list(vars = "hp",  type = "s",  bs = "tp", k = NULL),
    list(vars = c("wt", "hp"), type = "ti", bs = NULL, k = 5L)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  expect_s3_class(res, "mgcvUI_result")
  expect_s3_class(res$model, "gam")
  # The model should contain a ti term in the smooth table
  sm <- summary(res$model)
  expect_true(any(grepl("ti\\(", rownames(sm$s.table))))
})

test_that("fit_gam with te() tensor product term", {
  specs <- list(
    list(vars = c("wt", "hp"), type = "te", bs = NULL, k = 5L)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  expect_s3_class(res$model, "gam")
  # The model should contain a te term in the smooth table
  sm <- summary(res$model)
  expect_true(any(grepl("te\\(", rownames(sm$s.table))))
})

test_that("fit_gam with ti() plus main effects produces interpretable decomposition", {
  specs <- list(
    list(vars = "wt",  type = "s",  bs = "tp", k = NULL),
    list(vars = "disp", type = "s",  bs = "tp", k = NULL),
    list(vars = c("wt", "disp"), type = "ti", bs = NULL, k = 4L)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  sm <- summary(res$model)
  # Should have three smooth terms: s(wt), s(disp), ti(wt,disp)
  expect_true(nrow(sm$s.table) >= 3L)
})

# ---------------------------------------------------------------------------
# 2. Factor-by-smooth interactions: s(x, by = factor)
# ---------------------------------------------------------------------------

test_that("fit_gam with factor-by-smooth interaction", {
  df <- mtcars
  df$cyl_f <- factor(df$cyl)
  # The by-variable must also appear as a linear term so fit_gam

  # includes it in model_data (best practice for factor-by-smooth)
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = 5L, by = "cyl_f"),
    list(vars = "cyl_f", type = "linear", bs = NULL, k = NULL)
  )
  res <- fit_gam(df, "mpg", specs, cv_folds = 0)
  expect_s3_class(res$model, "gam")
  # Should have one smooth per factor level (3 levels for cyl)
  sm <- summary(res$model)
  expect_true(nrow(sm$s.table) >= 2L)
})

test_that("fit_gam with factor-by-smooth plus main factor effect", {
  df <- mtcars
  df$cyl_f <- factor(df$cyl)
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = 5L, by = "cyl_f"),
    list(vars = "cyl_f", type = "linear", bs = NULL, k = NULL)
  )
  res <- fit_gam(df, "mpg", specs, cv_folds = 0)
  expect_s3_class(res$model, "gam")
  # Parametric table should include the factor
  param_names <- rownames(summary(res$model)$p.table)
  expect_true(any(grepl("cyl_f", param_names)))
})

# ---------------------------------------------------------------------------
# 3. Response transforms (log, log10)
# ---------------------------------------------------------------------------

test_that("fit_gam with log-transformed response", {
  df <- mtcars
  df$mpg_log <- log(df$mpg)
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "hp", type = "s", bs = "tp", k = NULL)
  )
  res <- fit_gam(df, "mpg_log", specs, cv_folds = 0)
  expect_s3_class(res$model, "gam")
  # Fitted values should be on log scale (roughly 2-4 for mpg 10-55)
  fv <- fitted(res$model)
  expect_true(all(fv < 10))
  expect_true(all(fv > 0))
  # Back-transform should recover original scale
  bt <- earthUI:::back_transform_(fv, "log")
  expect_true(all(bt > 5 & bt < 60))
})

test_that("fit_gam with log10-transformed response", {
  df <- mtcars
  df$mpg_log10 <- log10(df$mpg)
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL)
  )
  res <- fit_gam(df, "mpg_log10", specs, cv_folds = 0)
  expect_s3_class(res$model, "gam")
  fv <- fitted(res$model)
  # log10 of 10-34 is roughly 1.0-1.5
  expect_true(all(fv > 0.5 & fv < 2.0))
  bt <- earthUI:::back_transform_(fv, "log10")
  expect_true(all(bt > 5 & bt < 60))
})

test_that("back_transform_ and apply_transform_ are inverses", {
  x <- c(10, 20, 33.9)
  for (xform in c("log", "log10", "none")) {
    transformed <- earthUI:::apply_transform_(x, xform)
    recovered   <- earthUI:::back_transform_(transformed, xform)
    expect_equal(recovered, x, tolerance = 1e-10,
                 label = paste("round-trip for", xform))
  }
})

test_that("apply_transform_ with NULL defaults to identity", {
  x <- c(5, 10, 20)
  expect_equal(earthUI:::apply_transform_(x, NULL), x)
  expect_equal(earthUI:::back_transform_(x, NULL), x)
})

# ---------------------------------------------------------------------------
# 4. Weights
# ---------------------------------------------------------------------------

test_that("fit_gam with observation weights", {
  wt <- rep(1, nrow(mtcars))
  wt[1:5] <- 10  # heavily weight first 5 observations
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0, weights = wt)
  expect_s3_class(res$model, "gam")
  # Model should have prior.weights stored
  expect_true(!is.null(res$model$prior.weights))
  expect_equal(length(res$model$prior.weights), nrow(mtcars))
})

test_that("fit_gam with weights changes fit compared to unweighted", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL)
  )
  res_no_wt <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  # Create weights that heavily emphasize heavy cars
  wts <- ifelse(mtcars$wt > 3.5, 10, 1)
  res_wt <- fit_gam(mtcars, "mpg", specs, cv_folds = 0, weights = wts)

  # Fitted values should differ
  expect_false(isTRUE(all.equal(fitted(res_no_wt$model),
                                fitted(res_wt$model))))
})

# ---------------------------------------------------------------------------
# 5. Multiple basis types: cr, ps, bs
# ---------------------------------------------------------------------------

test_that("fit_gam with cr basis", {
  specs <- list(list(vars = "wt", type = "s", bs = "cr", k = 5L))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  expect_s3_class(res$model, "gam")
})

test_that("fit_gam with ps basis", {
  specs <- list(list(vars = "wt", type = "s", bs = "ps", k = 8L))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  expect_s3_class(res$model, "gam")
})

test_that("fit_gam with bs (B-spline) basis", {
  specs <- list(list(vars = "wt", type = "s", bs = "bs", k = 8L))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  expect_s3_class(res$model, "gam")
})

test_that("fit_gam with mixed basis types in one model", {
  specs <- list(
    list(vars = "wt",   type = "s", bs = "cr", k = 5L),
    list(vars = "hp",   type = "s", bs = "ps", k = 8L),
    list(vars = "disp", type = "s", bs = "tp", k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  expect_s3_class(res$model, "gam")
  sm <- summary(res$model)
  expect_equal(nrow(sm$s.table), 3L)
})

# ---------------------------------------------------------------------------
# 6. Mixed smooth + linear + factor
# ---------------------------------------------------------------------------

test_that("fit_gam with smooth, linear, and factor terms combined", {
  df <- mtcars
  df$cyl_f <- factor(df$cyl)
  df$am_f  <- factor(df$am)
  specs <- list(
    list(vars = "wt",   type = "s",      bs = "tp", k = NULL),
    list(vars = "hp",   type = "s",      bs = "tp", k = NULL),
    list(vars = "disp", type = "linear", bs = NULL, k = NULL),
    list(vars = "cyl_f", type = "linear", bs = NULL, k = NULL),
    list(vars = "am_f",  type = "linear", bs = NULL, k = NULL)
  )
  res <- fit_gam(df, "mpg", specs, cv_folds = 0)
  expect_s3_class(res$model, "gam")
  sm <- summary(res$model)
  # Two smooth terms
  expect_equal(nrow(sm$s.table), 2L)
  # Parametric table should include disp, cyl_f levels, am_f levels
  pnames <- rownames(sm$p.table)
  expect_true(any(grepl("disp", pnames)))
  expect_true(any(grepl("cyl_f", pnames)))
})

# ---------------------------------------------------------------------------
# 7. Large k values
# ---------------------------------------------------------------------------

test_that("fit_gam with large k value", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = 20L)
  )
  # mtcars has 32 rows, 27 unique wt values, so k=20 should work
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  expect_s3_class(res$model, "gam")
  # Basis dimension should be 20
  expect_equal(res$model$smooth[[1]]$bs.dim, 20L)
})

test_that("fit_gam caps k when it exceeds unique values", {
  # Use a variable with few unique values
  specs <- list(
    list(vars = "gear", type = "s", bs = "tp", k = 15L)
  )
  # gear has only 3 unique values; fit_gam should cap k
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  expect_s3_class(res$model, "gam")
})

# ---------------------------------------------------------------------------
# 8. Optimizer parameter
# ---------------------------------------------------------------------------

test_that("fit_gam with outer/bfgs optimizer", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0,
                 optimizer = "outer_bfgs")
  expect_s3_class(res$model, "gam")
})

test_that("fit_gam with efs optimizer", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0,
                 optimizer = "efs")
  expect_s3_class(res$model, "gam")
})

test_that("fit_gam with default (outer_newton) optimizer", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0,
                 optimizer = "outer_newton")
  expect_s3_class(res$model, "gam")
})

# ---------------------------------------------------------------------------
# 9. Scale parameter
# ---------------------------------------------------------------------------

test_that("fit_gam with estimated scale (default, scale=0)", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0, scale = 0)
  expect_s3_class(res$model, "gam")
  # Scale should be estimated (positive)
  expect_true(res$model$scale > 0)
})

test_that("fit_gam with fixed scale parameter", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0, scale = 10)
  expect_s3_class(res$model, "gam")
  # Fixed scale should be reflected
  expect_equal(res$model$scale, 10, tolerance = 0.01)
})

# ---------------------------------------------------------------------------
# 10. Discrete parameter
# ---------------------------------------------------------------------------

test_that("fit_gam with discrete=TRUE for fast fitting", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "hp", type = "s", bs = "tp", k = NULL)
  )
  # discrete=TRUE with gam() internally calls bam(); REML produces a
  # warning about needing fREML/NCV but still fits successfully
  expect_warning(
    res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0, discrete = TRUE),
    "fREML"
  )
  expect_s3_class(res$model, "gam")
  # Model should still produce reasonable fitted values
  r2 <- summary(res$model)$r.sq
  expect_true(r2 > 0)
})

test_that("fit_gam with discrete=TRUE and nthreads", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  # discrete=TRUE with gam() internally calls bam(); REML produces a
  # warning about needing fREML/NCV but still fits successfully
  expect_warning(
    res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0,
                   discrete = TRUE, nthreads = 1L),
    "fREML"
  )
  expect_s3_class(res$model, "gam")
})

# ---------------------------------------------------------------------------
# 11. Formula building: factor-by-smooth
# ---------------------------------------------------------------------------

test_that("build_gam_formula produces by= clause for factor-by-smooth", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = 5L, by = "cyl_f")
  )
  f <- build_gam_formula("mpg", specs)
  f_str <- deparse(f)
  expect_true(grepl("by = cyl_f", f_str))
  expect_true(grepl("s\\(wt", f_str))
})

test_that("build_gam_formula with by='' (empty string) omits by clause", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = 5L, by = "")
  )
  f <- build_gam_formula("mpg", specs)
  f_str <- deparse(f)
  expect_false(grepl("by =", f_str))
})

test_that("build_gam_formula with by=NULL omits by clause", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = 5L, by = NULL)
  )
  f <- build_gam_formula("mpg", specs)
  f_str <- deparse(f)
  expect_false(grepl("by =", f_str))
})

# ---------------------------------------------------------------------------
# 12. Formula building: tensor products (te/ti)
# ---------------------------------------------------------------------------

test_that("build_gam_formula with ti() includes both variables", {
  specs <- list(
    list(vars = c("wt", "hp"), type = "ti", bs = NULL, k = 4L)
  )
  f <- build_gam_formula("mpg", specs)
  f_str <- deparse(f)
  expect_true(grepl("ti\\(wt, hp", f_str))
  expect_true(grepl("k = 4", f_str))
})

test_that("build_gam_formula with te() includes both variables", {
  specs <- list(
    list(vars = c("wt", "hp"), type = "te", bs = NULL, k = NULL)
  )
  f <- build_gam_formula("mpg", specs)
  f_str <- deparse(f)
  expect_true(grepl("te\\(wt, hp", f_str))
})

test_that("build_gam_formula with te() and marginal_bs includes bs argument", {
  specs <- list(
    list(vars = c("wt", "hp"), type = "te", bs = NULL, k = 5L,
         marginal_bs = c("tp", "tp"))
  )
  f <- build_gam_formula("mpg", specs)
  f_str <- deparse(f)
  expect_true(grepl("te\\(wt, hp", f_str))
  expect_true(grepl("bs = c\\(", f_str))
})

test_that("build_gam_formula with ti() and three variables", {
  specs <- list(
    list(vars = c("wt", "hp", "disp"), type = "ti", bs = NULL, k = 4L)
  )
  f <- build_gam_formula("mpg", specs)
  f_str <- deparse(f)
  expect_true(grepl("ti\\(wt, hp, disp", f_str))
})

# ---------------------------------------------------------------------------
# 13. build_gam_knots with cr basis
# ---------------------------------------------------------------------------

test_that("build_gam_knots returns correct knots for cr basis", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "cr", k = 4L)
  )
  ek <- structure(
    list(
      knots = list(wt = c(2.0, 3.0, 4.0, 5.0)),
      signs = list(wt = c(-1L, -1L, -1L, -1L))
    ),
    class = "mgcvUI_earth_knots"
  )
  result <- build_gam_knots(specs, ek, data = mtcars)
  expect_true(!is.null(result))
  expect_true("wt" %in% names(result))
  expect_equal(length(result$wt), 4L)
})

test_that("build_gam_knots augments when k > earth knots for cr basis", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "cr", k = 8L)
  )
  ek <- structure(
    list(
      knots = list(wt = c(2.5, 3.5, 4.5)),
      signs = list(wt = c(-1L, -1L, -1L))
    ),
    class = "mgcvUI_earth_knots"
  )
  result <- build_gam_knots(specs, ek, data = mtcars)
  expect_true(!is.null(result))
  # Should be augmented to exactly k=8 knots
  expect_equal(length(result$wt), 8L)
  # Original earth knots should still be present (or close)
  for (ek_val in c(2.5, 3.5, 4.5)) {
    expect_true(any(abs(result$wt - ek_val) < 0.01))
  }
})

test_that("build_gam_knots subsamples when earth knots exceed k for cr basis", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "cr", k = 3L)
  )
  ek <- structure(
    list(
      knots = list(wt = c(1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0)),
      signs = list(wt = rep(-1L, 8))
    ),
    class = "mgcvUI_earth_knots"
  )
  result <- build_gam_knots(specs, ek, data = mtcars)
  expect_true(!is.null(result))
  expect_equal(length(result$wt), 3L)
})

test_that("build_gam_knots ignores tp basis (no knots passed)", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = 5L)
  )
  ek <- structure(
    list(
      knots = list(wt = c(2.0, 3.0, 4.0)),
      signs = list(wt = c(-1L, -1L, -1L))
    ),
    class = "mgcvUI_earth_knots"
  )
  result <- build_gam_knots(specs, ek, data = mtcars)
  expect_null(result)
})

# ---------------------------------------------------------------------------
# 14. Predictions and R-squared
# ---------------------------------------------------------------------------

test_that("fitted values are reasonable (R-squared > 0) for mtcars", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "hp", type = "s", bs = "tp", k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  r2 <- summary(res$model)$r.sq
  expect_true(r2 > 0)
  # wt + hp should explain mpg well

  expect_true(r2 > 0.7)
})

test_that("predictions from predict() match fitted values", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  fv <- fitted(res$model)
  pv <- predict(res$model, newdata = mtcars)
  expect_equal(as.numeric(fv), as.numeric(pv), tolerance = 1e-6)
})

test_that("residuals sum to approximately zero", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  resids <- residuals(res$model)
  expect_true(abs(sum(resids)) < 1e-6)
})

# ---------------------------------------------------------------------------
# 15. Edge case: single predictor with k=3 (minimal smooth)
# ---------------------------------------------------------------------------

test_that("fit_gam with k=3 (minimal smooth)", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = 3L)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  expect_s3_class(res$model, "gam")
  # Basis dimension should be 3
  expect_equal(res$model$smooth[[1]]$bs.dim, 3L)
})

test_that("fit_gam with k=3 still fits a meaningful smooth", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = 3L)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  r2 <- summary(res$model)$r.sq
  expect_true(r2 > 0.5)  # wt alone explains mpg well even with k=3
})

# ---------------------------------------------------------------------------
# Additional advanced scenarios
# ---------------------------------------------------------------------------

test_that("fit_gam with iris dataset (different data)", {
  specs <- list(
    list(vars = "Sepal.Width",  type = "s", bs = "tp", k = NULL),
    list(vars = "Petal.Length", type = "s", bs = "tp", k = NULL)
  )
  res <- fit_gam(iris, "Sepal.Length", specs, cv_folds = 0)
  expect_s3_class(res$model, "gam")
  r2 <- summary(res$model)$r.sq
  expect_true(r2 > 0)
})

test_that("fit_gam with iris factor (Species) as linear term", {
  specs <- list(
    list(vars = "Petal.Length", type = "s",      bs = "tp", k = NULL),
    list(vars = "Species",     type = "linear", bs = NULL, k = NULL)
  )
  res <- fit_gam(iris, "Sepal.Length", specs, cv_folds = 0)
  expect_s3_class(res$model, "gam")
  pnames <- rownames(summary(res$model)$p.table)
  expect_true(any(grepl("Species", pnames)))
})

test_that("fit_gam NULL parameters use defensive defaults", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  # Pass NULL for several parameters that have defensive defaults
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0,
                 method = NULL, select = NULL, gamma = NULL,
                 family = NULL, scale = NULL, discrete = NULL,
                 nthreads = NULL)
  expect_s3_class(res$model, "gam")
})

test_that("fit_gam errors on missing response column", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  expect_error(fit_gam(mtcars, "nonexistent", specs, cv_folds = 0),
               "not found in data")
})

test_that("fit_gam errors on missing predictor column", {
  specs <- list(list(vars = "nonexistent_var", type = "s", bs = "tp", k = NULL))
  expect_error(fit_gam(mtcars, "mpg", specs, cv_folds = 0),
               "not in data")
})

test_that("fit_gam handles character-valued factor columns correctly", {
  df <- mtcars
  df$cyl_chr <- as.character(df$cyl)
  specs <- list(
    list(vars = "wt",      type = "s",      bs = "tp", k = NULL),
    list(vars = "cyl_chr", type = "linear", bs = NULL, k = NULL)
  )
  res <- fit_gam(df, "mpg", specs, cv_folds = 0)
  expect_s3_class(res$model, "gam")
  # The character column should have been coerced to factor
  expect_true(is.factor(res$model$model$cyl_chr))
})

test_that("reconcile_knots_ switches cr to tp when fewer than 3 earth knots", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "cr", k = 5L)
  )
  ek <- structure(
    list(
      knots = list(wt = c(3.0, 4.0)),  # only 2 knots
      signs = list(wt = c(-1L, -1L))
    ),
    class = "mgcvUI_earth_knots"
  )
  result <- earthUI:::reconcile_knots_(specs, ek)
  # Should have switched to tp because too few knots for cr

  expect_equal(result[[1]]$bs, "tp")
})

test_that("reconcile_knots_ returns specs unchanged when no earth knots", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "cr", k = 5L),
    list(vars = "hp", type = "s", bs = "tp", k = NULL)
  )
  result <- earthUI:::reconcile_knots_(specs, NULL)
  expect_identical(result, specs)
})

test_that("fit_gam with te() produces different result than additive s() terms", {
  specs_additive <- list(
    list(vars = "wt", type = "s", bs = "tp", k = 5L),
    list(vars = "hp", type = "s", bs = "tp", k = 5L)
  )
  specs_tensor <- list(
    list(vars = c("wt", "hp"), type = "te", bs = NULL, k = 5L)
  )
  res_add <- fit_gam(mtcars, "mpg", specs_additive, cv_folds = 0)
  res_ten <- fit_gam(mtcars, "mpg", specs_tensor, cv_folds = 0)
  # AIC should differ between the two models
  expect_false(identical(AIC(res_add$model), AIC(res_ten$model)))
})

test_that("fit_gam formula is stored correctly in result", {
  specs <- list(
    list(vars = "wt",  type = "s",      bs = "tp", k = 5L),
    list(vars = "cyl", type = "linear", bs = NULL, k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  f_str <- deparse(res$formula)
  expect_true(grepl("mpg", f_str))
  expect_true(grepl("s\\(wt", f_str))
  expect_true(grepl("cyl", f_str))
})

test_that("fit_gam cv_rsq is NULL when cv_folds=0", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  expect_null(res$cv_rsq)
})

test_that("fit_gam earth_knots is NULL when not provided", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  expect_null(res$earth_knots)
})
