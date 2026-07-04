# --- format_glmnet_equation_ ---
test_that("format_glmnet_equation_ escapes underscores for LaTeX", {
  set.seed(42)
  x <- matrix(rnorm(100 * 3), 100, 3,
              dimnames = list(NULL, c("sale_price_adj", "lot_size", "age")))
  y <- x[, 1] * 2 + x[, 2] * (-1) + rnorm(100, sd = 0.5)
  fit <- glmnet::cv.glmnet(x, y)

  eq <- earthUI:::format_glmnet_equation_(fit, fit$lambda.1se, NULL,
                                            "sale_price")
  # Variable names with underscores should appear in \text{} blocks
  expect_true(grepl("sale_price", eq$latex),
              info = "Variable names should appear in equation")
  # Should be wrapped in array environment

  expect_true(grepl("\\\\begin\\{array\\}", eq$latex))
  expect_true(grepl("\\\\end\\{array\\}", eq$latex))
})

test_that("format_glmnet_equation_ works with relaxed model", {
  set.seed(42)
  x <- matrix(rnorm(200 * 3), 200, 3,
              dimnames = list(NULL, c("x1", "x2", "x3")))
  y <- x[, 1] * 3 + rnorm(200)
  fit <- glmnet::cv.glmnet(x, y, relax = TRUE)

  gamma_val <- fit$relaxed$gamma.1se
  lambda_val <- fit$relaxed$lambda.1se

  eq <- earthUI:::format_glmnet_equation_(fit, lambda_val, gamma_val, "y")
  expect_true(is.list(eq))
  expect_true(nzchar(eq$latex))
})

# --- prepare_report_assets_glmnet ---
test_that("prepare_report_assets_glmnet creates assets directory with expected contents", {
  set.seed(42)
  x <- matrix(rnorm(100 * 3), 100, 3,
              dimnames = list(NULL, c("x1", "x2", "x3")))
  y <- x[, 1] * 2 + x[, 2] * (-1) + rnorm(100, sd = 0.5)
  df <- data.frame(x, y = y)
  fit <- glmnet::cv.glmnet(x, y)
  lam <- fit$lambda.1se
  coefs <- coef(fit, s = lam)
  coef_df <- data.frame(Variable = rownames(coefs),
                         Coefficient = as.numeric(coefs),
                         stringsAsFactors = FALSE)

  assets_dir <- prepare_report_assets_glmnet(
    model = fit, lambda = lam, gamma = NULL,
    x_mat = x, y_vec = y, coef_df = coef_df,
    predictors = c("x1", "x2", "x3"), response = "y",
    data = df, data_file_name = "test.csv"
  )
  on.exit(unlink(assets_dir, recursive = TRUE))

  expect_true(dir.exists(assets_dir))
  expect_true(file.exists(file.path(assets_dir, "report_data.rds")))
  expect_true(dir.exists(file.path(assets_dir, "plots")))

  # Check essential plots exist
  plots <- list.files(file.path(assets_dir, "plots"))
  expect_true(any(grepl("^importance", plots)))
  expect_true(any(grepl("^coef_path", plots)))
  expect_true(any(grepl("^actual_vs_predicted", plots)))
  expect_true(any(grepl("^residuals_vs_fitted", plots)))
  expect_true(any(grepl("^qq", plots)))
  expect_true(any(grepl("^cv_error", plots)))

  # Check RDS contents
  assets <- readRDS(file.path(assets_dir, "report_data.rds"))
  expect_true(!is.null(assets$model))
  expect_true(!is.null(assets$coef_df))
  expect_true(!is.null(assets$importance_df))
  expect_true(!is.null(assets$anova_df))
  expect_equal(assets$response, "y")
  expect_equal(assets$n_obs, 100L)
})

test_that("prepare_report_assets_glmnet strips bloated Call: from cv.relaxed", {
  set.seed(42)
  x <- matrix(rnorm(200 * 3), 200, 3,
              dimnames = list(NULL, c("x1", "x2", "x3")))
  y <- x[, 1] * 3 + rnorm(200)
  fit <- do.call(glmnet::cv.glmnet, list(x = x, y = y, relax = TRUE))
  lam <- fit$lambda.1se
  coefs <- coef(fit, s = lam, gamma = 0)
  coef_df <- data.frame(Variable = rownames(coefs),
                         Coefficient = as.numeric(coefs),
                         stringsAsFactors = FALSE)

  assets_dir <- prepare_report_assets_glmnet(
    model = fit, lambda = lam, gamma = 0,
    x_mat = x, y_vec = y, coef_df = coef_df,
    predictors = c("x1", "x2", "x3"), response = "y",
    data = data.frame(x, y = y), data_file_name = "test.csv"
  )
  on.exit(unlink(assets_dir, recursive = TRUE))

  assets <- readRDS(file.path(assets_dir, "report_data.rds"))
  # The saved model's Call: should be short, not the full function body
  model_out <- utils::capture.output(print(assets$model))
  max_line <- max(nchar(model_out))
  expect_true(max_line < 1000,
              info = paste("Longest print line is", max_line,
                           "chars; should be <1000 after Call: stripping"))
})

test_that("prepare_report_assets_glmnet generates contribution plots for numeric vars", {
  set.seed(42)
  x <- matrix(rnorm(100 * 3), 100, 3,
              dimnames = list(NULL, c("living_sqft", "lot_size", "age")))
  y <- x[, 1] * 100 + x[, 2] * (-50) + rnorm(100, sd = 10)
  df <- data.frame(x, sale_price = y)
  fit <- glmnet::cv.glmnet(x, y)
  lam <- fit$lambda.1se
  coefs <- coef(fit, s = lam)
  coef_df <- data.frame(Variable = rownames(coefs),
                         Coefficient = as.numeric(coefs),
                         stringsAsFactors = FALSE)

  assets_dir <- prepare_report_assets_glmnet(
    model = fit, lambda = lam, gamma = NULL,
    x_mat = x, y_vec = y, coef_df = coef_df,
    predictors = colnames(x), response = "sale_price",
    data = df, data_file_name = "test.csv"
  )
  on.exit(unlink(assets_dir, recursive = TRUE))

  plots <- list.files(file.path(assets_dir, "plots"))
  # Should have scatter plots, not histograms
  expect_true(any(grepl("^contrib_living_sqft\\.", plots)))
  expect_true(any(grepl("^contrib_lot_size\\.", plots)))
})

# --- compute_contrib_for_report_ ---
test_that("compute_contrib_for_report_ handles interactions", {
  set.seed(42)
  x <- matrix(rnorm(100 * 3), 100, 3,
              dimnames = list(NULL, c("x1", "x2", "x3")))
  x_int <- cbind(x, "x1:x2" = x[, 1] * x[, 2])
  y <- x[, 1] * 2 + x[, 1] * x[, 2] * 0.5 + rnorm(100, sd = 0.5)
  fit <- glmnet::cv.glmnet(x_int, y)
  lam <- fit$lambda.min
  coefs <- coef(fit, s = lam)
  coef_vec <- as.numeric(coefs)[-1]
  names(coef_vec) <- rownames(coefs)[-1]

  result <- earthUI:::compute_contrib_for_report_(
    coef_vec, x_int, c("x1", "x2", "x3"),
    data.frame(x, y = y), NULL
  )

  # Should have entries for single predictors and the interaction
  expect_true(is.list(result$contribs))
  # At least x1 should have contributions
  has_x1 <- "x1" %in% names(result$contribs)
  expect_true(has_x1)
  # If the interaction term has nonzero coef, it should appear
  if (coef_vec["x1:x2"] != 0) {
    expect_true("x1:x2" %in% names(result$contribs))
  }
})

# --- render_report_glmnet ---
test_that("render_report_glmnet produces HTML output", {
  skip_on_cran()
  set.seed(42)
  x <- matrix(rnorm(100 * 3), 100, 3,
              dimnames = list(NULL, c("x1", "x2", "x3")))
  y <- x[, 1] * 2 + rnorm(100, sd = 0.5)
  df <- data.frame(x, y = y)
  fit <- glmnet::cv.glmnet(x, y)
  lam <- fit$lambda.1se
  coefs <- coef(fit, s = lam)
  coef_df <- data.frame(Variable = rownames(coefs),
                         Coefficient = as.numeric(coefs),
                         stringsAsFactors = FALSE)

  assets_dir <- prepare_report_assets_glmnet(
    model = fit, lambda = lam, gamma = NULL,
    x_mat = x, y_vec = y, coef_df = coef_df,
    predictors = c("x1", "x2", "x3"), response = "y",
    data = df, data_file_name = "test.csv"
  )
  on.exit(unlink(assets_dir, recursive = TRUE))

  out_html <- tempfile(fileext = ".html")
  render_report_glmnet(output_format = "html", output_file = out_html,
                assets_dir = assets_dir)
  expect_true(file.exists(out_html))
  expect_true(file.size(out_html) > 0)
})

# --- report assets across purpose modes + non-HTML render formats ---

# Shared fixture: build an assets directory for a given purpose and return its
# path. Caller is responsible for unlink().
.build_report_assets <- function(purpose = "general", response = "y") {
  set.seed(42)
  x <- matrix(stats::rnorm(100 * 3), 100, 3,
              dimnames = list(NULL, c("x1", "x2", "x3")))
  y <- x[, 1] * 2 + x[, 2] * (-1) + stats::rnorm(100, sd = 0.5)
  df <- data.frame(x, y = y)
  names(df)[ncol(df)] <- response
  fit <- glmnet::cv.glmnet(x, y)
  lam <- fit$lambda.1se
  coefs <- coef(fit, s = lam)
  coef_df <- data.frame(Variable = rownames(coefs),
                        Coefficient = as.numeric(coefs),
                        stringsAsFactors = FALSE)
  prepare_report_assets_glmnet(
    model = fit, lambda = lam, gamma = NULL,
    x_mat = x, y_vec = y, coef_df = coef_df,
    predictors = c("x1", "x2", "x3"), response = response,
    data = df, purpose = purpose, data_file_name = "test.csv"
  )
}

test_that("prepare_report_assets_glmnet records each purpose mode", {
  for (p in c("general", "appraisal", "market")) {
    assets_dir <- .build_report_assets(purpose = p)
    on.exit(unlink(assets_dir, recursive = TRUE), add = TRUE)
    expect_true(dir.exists(assets_dir))
    assets <- readRDS(file.path(assets_dir, "report_data.rds"))
    expect_equal(assets$purpose, p)
    expect_false(is.null(assets$model))
  }
})

test_that("render_report_glmnet produces DOCX output", {
  skip_on_cran()
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  assets_dir <- .build_report_assets()
  on.exit(unlink(assets_dir, recursive = TRUE), add = TRUE)

  out_docx <- tempfile(fileext = ".docx")
  render_report_glmnet(output_format = "docx", output_file = out_docx,
                assets_dir = assets_dir)
  expect_true(file.exists(out_docx))
  expect_gt(file.size(out_docx), 0)
})

test_that("render_report_glmnet produces PDF output", {
  skip_on_cran()
  skip_if_not(earthUI:::has_latex_(), "no LaTeX installation")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  assets_dir <- .build_report_assets()
  on.exit(unlink(assets_dir, recursive = TRUE), add = TRUE)

  out_pdf <- tempfile(fileext = ".pdf")
  render_report_glmnet(output_format = "pdf", output_file = out_pdf,
                assets_dir = assets_dir)
  expect_true(file.exists(out_pdf))
  expect_gt(file.size(out_pdf), 0)
})
