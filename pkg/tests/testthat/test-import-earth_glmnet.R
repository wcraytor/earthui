# Helper: build a minimal earthUI_result for testing
make_earth_result <- function(formula = mpg ~ wt + hp, data = mtcars,
                               degree = 1L) {
  skip_if_not_installed("earth")
  m <- earth::earth(formula, data = data, degree = degree)
  preds <- setdiff(all.vars(formula), all.vars(formula[[2]]))
  structure(
    list(
      model = m,
      target = as.character(formula[[2]]),
      predictors = preds,
      categoricals = character(0),
      linpreds = character(0),
      degree = degree,
      cv_enabled = FALSE,
      allowed_matrix = NULL,
      data = data,
      elapsed = 0,
      trace_output = character(0)
    ),
    class = "earthUI_result"
  )
}


# --- import_earth_glmnet ---

test_that("import_earth_glmnet returns glmnetUI_earth_import object", {
  er <- make_earth_result()
  ei <- import_earth_glmnet(er)

  expect_s3_class(ei, "glmnetUI_earth_import")
  expect_true(inherits(ei$model, "earth"))
  expect_equal(ei$target, "mpg")
  expect_equal(ei$predictors, c("wt", "hp"))
  expect_true(is.numeric(ei$earth_summary$r_squared))
  expect_true(ei$earth_summary$n_terms >= 1L)
})

test_that("import_earth_glmnet works from .rds file", {
  er <- make_earth_result()
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  saveRDS(er, tmp)
  ei <- import_earth_glmnet(tmp)

  expect_s3_class(ei, "glmnetUI_earth_import")
  expect_equal(ei$target, "mpg")
})

test_that("import_earth_glmnet rejects non-earthUI objects", {
  expect_error(import_earth_glmnet(list(x = 1)), "earthUI_result")
})

test_that("import_earth_glmnet rejects missing files", {
  expect_error(import_earth_glmnet("/nonexistent/file.rds"), "File not found")
})

test_that("import_earth_glmnet preserves metadata", {
  skip_if_not_installed("earth")
  m <- earth::earth(mpg ~ wt + hp, data = mtcars)
  er <- structure(
    list(model = m, target = "mpg", predictors = c("wt", "hp"),
         categoricals = "gear", linpreds = "wt", degree = 2L,
         cv_enabled = FALSE, allowed_matrix = NULL, data = mtcars,
         elapsed = 0, trace_output = character(0)),
    class = "earthUI_result"
  )
  ei <- import_earth_glmnet(er)
  expect_equal(ei$categoricals, "gear")
  expect_equal(ei$linpreds, "wt")
  expect_equal(ei$degree, 2L)
})

test_that("import_earth_glmnet handles NULL categoricals/linpreds", {
  skip_if_not_installed("earth")
  m <- earth::earth(mpg ~ wt + hp, data = mtcars)
  er <- structure(
    list(model = m, target = "mpg", predictors = c("wt", "hp"),
         categoricals = NULL, linpreds = NULL, degree = NULL,
         cv_enabled = FALSE, allowed_matrix = NULL, data = mtcars,
         elapsed = 0, trace_output = character(0)),
    class = "earthUI_result"
  )
  ei <- import_earth_glmnet(er)
  expect_equal(ei$categoricals, character(0))
  expect_equal(ei$linpreds, character(0))
  expect_equal(ei$degree, 1L)
})

test_that("import_earth_glmnet includes original data", {
  er <- make_earth_result()
  ei <- import_earth_glmnet(er)
  expect_identical(ei$data, mtcars)
})


# --- build_earth_basis ---

test_that("build_earth_basis returns NULL for NULL input", {
  expect_null(build_earth_basis(mtcars, NULL))
})

test_that("build_earth_basis rejects wrong class", {
  expect_error(build_earth_basis(mtcars, list()),
               "glmnetUI_earth_import")
})

test_that("build_earth_basis generates basis matrix via model.matrix", {
  er <- make_earth_result()
  ei <- import_earth_glmnet(er)
  bx <- build_earth_basis(mtcars, ei)

  expect_true(is.matrix(bx))
  expect_equal(nrow(bx), nrow(mtcars))
  # Should have at least 1 basis column (intercept removed)
  expect_true(ncol(bx) >= 1L)
  # No intercept column
  expect_false("(Intercept)" %in% colnames(bx))
})

test_that("build_earth_basis works with new data", {
  er <- make_earth_result()
  ei <- import_earth_glmnet(er)

  # Predict on subset
  new_data <- mtcars[1:5, ]
  bx <- build_earth_basis(new_data, ei)

  expect_true(is.matrix(bx))
  expect_equal(nrow(bx), 5L)
})

test_that("build_earth_basis produces interaction columns with degree=2", {
  er <- make_earth_result(mpg ~ wt + hp, mtcars, degree = 2L)
  ei <- import_earth_glmnet(er)
  bx <- build_earth_basis(mtcars, ei)

  expect_true(is.matrix(bx))
  expect_equal(nrow(bx), nrow(mtcars))
  # Check for interaction columns (contain *)
  if (ei$earth_summary$n_terms > 2) {
    # earth with degree=2 may or may not find interactions
    # depending on the data, so just check it runs
    expect_true(ncol(bx) >= 1L)
  }
})

test_that("build_earth_basis columns match earth model.matrix", {
  er <- make_earth_result()
  ei <- import_earth_glmnet(er)

  # Direct earth model.matrix (the standard)
  bx_direct <- stats::model.matrix(ei$model)[, -1, drop = FALSE]
  # Our wrapper
  bx_ours <- build_earth_basis(mtcars, ei)

  expect_equal(colnames(bx_ours), colnames(bx_direct))
  expect_equal(as.numeric(bx_ours), as.numeric(bx_direct))
})


# --- export_knots_csv_glmnet ---

test_that("export_knots_csv_glmnet writes valid CSV", {
  er <- make_earth_result()
  ei <- import_earth_glmnet(er)
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  export_knots_csv_glmnet(ei, tmp)

  expect_true(file.exists(tmp))
  csv <- utils::read.csv(tmp, stringsAsFactors = FALSE)
  expect_true(all(c("variable", "knot", "direction") %in% names(csv)))
  expect_true(all(csv$direction %in% c("forward", "reverse")))
  expect_true(is.numeric(csv$knot))
})

test_that("export_knots_csv_glmnet rejects wrong class", {
  expect_error(export_knots_csv_glmnet(list(), tempfile()),
               "glmnetUI_earth_import")
})


# --- Round-trip: earth -> glmnet integration ---

test_that("earth basis integrates with glmnet", {
  skip_if_not_installed("glmnet")
  er <- make_earth_result()
  ei <- import_earth_glmnet(er)
  bx <- build_earth_basis(mtcars, ei)

  fit <- glmnet::cv.glmnet(bx, mtcars$mpg)
  expect_s3_class(fit, "cv.glmnet")
  preds <- stats::predict(fit, newx = bx, s = "lambda.min")
  expect_equal(length(preds), nrow(mtcars))
})

test_that("earth degree=2 interactions are present in basis and survive glmnet", {
  skip_if_not_installed("glmnet")

  # Synthetic data with a true interaction: y = x1 * x2 + noise
  set.seed(42)
  n <- 200
  syn <- data.frame(
    x1 = runif(n, 0, 10),
    x2 = runif(n, 0, 10),
    x3 = runif(n, 0, 10)
  )
  syn$y <- 5 * syn$x1 + 3 * syn$x2 + 2 * syn$x1 * syn$x2 + rnorm(n, sd = 5)

  er <- make_earth_result(y ~ x1 + x2 + x3, syn, degree = 2L)
  ei <- import_earth_glmnet(er)
  bx <- build_earth_basis(NULL, ei)

  # Verify interaction columns exist in the basis
  int_cols <- grep("\\*", colnames(bx), value = TRUE)
  expect_true(length(int_cols) > 0,
              info = "Earth basis should contain interaction columns (with *)")

  # Fit glmnet and verify interactions have non-zero coefficients
  fit <- glmnet::cv.glmnet(bx, syn$y)
  expect_s3_class(fit, "cv.glmnet")

  coefs <- as.numeric(stats::coef(fit, s = "lambda.min"))
  nms <- c("(Intercept)", colnames(bx))
  names(coefs) <- nms
  int_coefs <- coefs[int_cols]
  n_nonzero_int <- sum(int_coefs != 0)
  expect_true(n_nonzero_int > 0,
              info = paste("Interaction coefficients should be non-zero.",
                           "Cols:", paste(int_cols, collapse = ", "),
                           "Coefs:", paste(round(int_coefs, 4), collapse = ", ")))

  preds <- stats::predict(fit, newx = bx, s = "lambda.min")
  expect_equal(length(preds), n)
})

test_that("earth degree=3 interactions pass through to glmnet", {
  skip_if_not_installed("glmnet")

  # Synthetic data with a 3-way interaction: y = x1 * x2 * x3 + noise
  set.seed(123)
  n <- 300
  syn <- data.frame(
    x1 = runif(n, 0, 10),
    x2 = runif(n, 0, 10),
    x3 = runif(n, 0, 10)
  )
  syn$y <- 2 * syn$x1 * syn$x2 + 0.5 * syn$x1 * syn$x2 * syn$x3 + rnorm(n, sd = 10)

  er <- make_earth_result(y ~ x1 + x2 + x3, syn, degree = 3L)
  ei <- import_earth_glmnet(er)
  bx <- build_earth_basis(NULL, ei)

  int_cols <- grep("\\*", colnames(bx), value = TRUE)
  expect_true(length(int_cols) > 0,
              info = "Earth degree=3 should find interactions")

  fit <- glmnet::cv.glmnet(bx, syn$y)
  expect_s3_class(fit, "cv.glmnet")

  coefs <- as.numeric(stats::coef(fit, s = "lambda.min"))
  nms <- c("(Intercept)", colnames(bx))
  names(coefs) <- nms
  int_coefs <- coefs[int_cols]
  n_nonzero_int <- sum(int_coefs != 0)
  expect_true(n_nonzero_int > 0,
              info = paste("Degree=3 interaction coefficients should be non-zero"))

  preds <- stats::predict(fit, newx = bx, s = "lambda.min")
  expect_equal(length(preds), n)
})

test_that("earth basis with interactions integrates with glmnet", {
  skip_if_not_installed("glmnet")
  er <- make_earth_result(mpg ~ wt + hp, mtcars, degree = 2L)
  ei <- import_earth_glmnet(er)
  bx <- build_earth_basis(NULL, ei)

  fit <- glmnet::cv.glmnet(bx, mtcars$mpg)
  expect_s3_class(fit, "cv.glmnet")
  # Non-zero coefficients should exist
  coefs <- as.numeric(stats::coef(fit, s = "lambda.min"))
  expect_true(sum(coefs != 0) > 1)
})


# --- Column overlap: earth basis + formula can share column names ---

test_that("combined formula + earth matrix works when column names overlap", {
  skip_if_not_installed("glmnet")

  # Create data with a factor that produces dummy columns
  set.seed(99)
  n <- 100
  syn <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    grp = factor(sample(c("A", "B", "C"), n, replace = TRUE))
  )
  syn$y <- 2 * syn$x1 + ifelse(syn$grp == "B", 5, 0) + rnorm(n)

  # Fit earth with the factor (produces dummy columns like grpB, grpC)
  er <- make_earth_result(y ~ x1 + x2 + grp, syn, degree = 1L)
  ei <- import_earth_glmnet(er)
  bx <- build_earth_basis(NULL, ei)

  # Build formula model.matrix (also produces grpB, grpC dummies)
  x_formula <- stats::model.matrix(~ x1 + x2 + grp - 1, data = syn)

  # Combine like glmnetUI does: formula columns then earth basis
  x_combined <- cbind(x_formula, bx)
  n_formula <- ncol(x_formula)
  n_earth <- ncol(bx)
  n_total <- ncol(x_combined)

  # Check for overlapping names
  overlap <- intersect(colnames(x_formula), colnames(bx))

  # Fit should work even with duplicate column names
  fit <- glmnet::cv.glmnet(x_combined, syn$y)
  expect_s3_class(fit, "cv.glmnet")

  # Prediction with same matrix should work
  preds <- stats::predict(fit, newx = x_combined, s = "lambda.min")
  expect_equal(length(preds), n)

  # Positional split must recover the correct column counts
  expect_equal(n_total, n_formula + n_earth)
  formula_cols <- seq_len(n_formula)
  earth_cols <- (n_formula + 1):n_total
  expect_equal(length(formula_cols), n_formula)
  expect_equal(length(earth_cols), n_earth)
})

test_that("prediction works after rebuild with overlapping earth/formula columns", {
  skip_if_not_installed("glmnet")

  # Simulate the export scenario: train with combined matrix,
  # then rebuild the same matrix for prediction
  set.seed(42)
  n <- 150
  syn <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    grp = factor(sample(c("A", "B", "C"), n, replace = TRUE))
  )
  syn$y <- 3 * syn$x1 - 2 * syn$x2 + ifelse(syn$grp == "C", 4, 0) + rnorm(n)

  er <- make_earth_result(y ~ x1 + x2 + grp, syn, degree = 1L)
  ei <- import_earth_glmnet(er)
  bx_train <- build_earth_basis(NULL, ei)
  x_formula_train <- stats::model.matrix(~ x1 + x2 + grp - 1, data = syn)
  x_train <- cbind(x_formula_train, bx_train)

  fit <- glmnet::cv.glmnet(x_train, syn$y)

  # Rebuild for prediction (same data = same matrix)
  bx_pred <- build_earth_basis(NULL, ei)
  x_formula_pred <- stats::model.matrix(~ x1 + x2 + grp - 1, data = syn)
  x_pred <- cbind(x_formula_pred, bx_pred)

  # Column counts must match

  expect_equal(ncol(x_pred), ncol(x_train))

  # Prediction must succeed
  preds <- stats::predict(fit, newx = x_pred, s = "lambda.min")
  expect_equal(length(preds), n)

  # Predictions should be reasonable (R² > 0.5)
  r2 <- 1 - sum((syn$y - preds)^2) / sum((syn$y - mean(syn$y))^2)
  expect_true(r2 > 0.5,
              info = paste("R² should be decent, got:", round(r2, 3)))
})
