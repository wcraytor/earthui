# --- Launch function ---
test_that("glmnetUI exists and is a function", {
  expect_true(is.function(launch_glmnet))
})

test_that("glmnetUI has expected default arguments", {
  args <- formals(launch_glmnet)
  expect_true("port" %in% names(args))
  expect_equal(args$port, 7879L)
})

# --- to_snake_case ---
test_that("to_snake_case converts camelCase", {
  expect_equal(to_snake_case("SalePrice"), "sale_price")
  expect_equal(to_snake_case("totalSqFt"), "total_sq_ft")
})

test_that("to_snake_case converts dots and spaces", {
  expect_equal(to_snake_case("Lot.Size"), "lot_size")
  expect_equal(to_snake_case("Total Sq Ft"), "total_sq_ft")
})

test_that("to_snake_case lowercases all-caps", {
  expect_equal(to_snake_case("AGE"), "age")
})

test_that("to_snake_case preserves snake_case", {
  expect_equal(to_snake_case("already_snake"), "already_snake")
})

test_that("to_snake_case strips leading/trailing whitespace", {
  expect_equal(to_snake_case("  Leading Spaces  "), "leading_spaces")
})

test_that("to_snake_case handles duplicates", {
  result <- to_snake_case(c("Sale Price", "sale_price"))
  expect_equal(length(unique(result)), 2)
})

test_that("to_snake_case handles empty input", {
  expect_equal(to_snake_case(character(0)), character(0))
})

test_that("to_snake_case collapses multiple underscores", {
  expect_equal(to_snake_case("a__b"), "a_b")
  expect_equal(to_snake_case("foo---bar"), "foo_bar")
})

# --- detect_column_types_glmnet ---
test_that("detect_column_types_glmnet classifies numeric correctly", {
  df <- data.frame(x = c(1.5, 2.5, 3.5))
  expect_equal(detect_column_types_glmnet(df)[["x"]], "numeric")
})

test_that("detect_column_types_glmnet classifies integer correctly", {
  df <- data.frame(x = 1L:5L)
  expect_equal(detect_column_types_glmnet(df)[["x"]], "integer")
})

test_that("detect_column_types_glmnet classifies factor correctly", {
  df <- data.frame(x = factor(c("a", "b", "a")))
  expect_equal(detect_column_types_glmnet(df)[["x"]], "factor")
})

test_that("detect_column_types_glmnet classifies Date correctly", {
  df <- data.frame(x = as.Date("2024-01-01") + 0:4)
  expect_equal(detect_column_types_glmnet(df)[["x"]], "Date")
})

test_that("detect_column_types_glmnet classifies POSIXct correctly", {
  df <- data.frame(x = as.POSIXct("2024-01-01") + 0:4)
  expect_equal(detect_column_types_glmnet(df)[["x"]], "POSIXct")
})

test_that("detect_column_types_glmnet classifies character correctly", {
  df <- data.frame(x = paste0("val_", 1:20), stringsAsFactors = FALSE)
  expect_equal(detect_column_types_glmnet(df)[["x"]], "character")
})

test_that("detect_column_types_glmnet auto-detects low-cardinality character as factor", {
  df <- data.frame(x = rep(c("A", "B"), 10), stringsAsFactors = FALSE)
  expect_equal(detect_column_types_glmnet(df)[["x"]], "factor")
})

test_that("detect_column_types_glmnet handles multiple columns", {
  df <- data.frame(
    num = c(1.5, 2.5, 3.5, 4.5, 5.5),
    int = 1L:5L,
    char = paste0("v_", 1:5),
    fac = factor(c("x", "y", "x", "y", "x")),
    dt = as.Date("2024-01-01") + 0:4,
    ts = as.POSIXct("2024-01-01 12:00:00") + 0:4,
    stringsAsFactors = FALSE
  )
  types <- detect_column_types_glmnet(df)
  expect_equal(types[["num"]], "numeric")
  expect_equal(types[["int"]], "integer")
  expect_equal(types[["fac"]], "factor")
  expect_equal(types[["dt"]], "Date")
  expect_equal(types[["ts"]], "POSIXct")
})

# --- check_sign_warnings ---
test_that("check_sign_warnings flags incorrect positive sign", {
  result <- check_sign_warnings(
    coef_names = c("sqft"),
    coef_values = c(-50),
    expected_signs = c(sqft = "positive")
  )
  expect_match(result, "Expected positive, got negative")
})

test_that("check_sign_warnings flags incorrect negative sign", {
  result <- check_sign_warnings(
    coef_names = c("age"),
    coef_values = c(10),
    expected_signs = c(age = "negative")
  )
  expect_match(result, "Expected negative, got positive")
})

test_that("check_sign_warnings returns OK for correct signs", {
  result <- check_sign_warnings(
    coef_names = c("sqft", "age"),
    coef_values = c(50, -2),
    expected_signs = c(sqft = "positive", age = "negative")
  )
  expect_equal(result, c("OK", "OK"))
})

test_that("check_sign_warnings handles 'either' correctly", {
  result <- check_sign_warnings(
    coef_names = c("x1", "x2"),
    coef_values = c(-5, 10),
    expected_signs = c(x1 = "either", x2 = "either")
  )
  expect_equal(result, c("OK", "OK"))
})

test_that("check_sign_warnings handles zero coefficients", {
  result <- check_sign_warnings(
    coef_names = c("x1"),
    coef_values = c(0),
    expected_signs = c(x1 = "positive")
  )
  expect_equal(result, "OK")
})

test_that("check_sign_warnings handles missing expected sign", {
  result <- check_sign_warnings(
    coef_names = c("x1"),
    coef_values = c(-5),
    expected_signs = c(other = "positive")
  )
  expect_equal(result, "OK")
})

test_that("check_sign_warnings handles empty expected signs", {
  result <- check_sign_warnings(
    coef_names = c("x1", "x2"),
    coef_values = c(1, -1),
    expected_signs = stats::setNames(character(0), character(0))
  )
  expect_equal(result, c("OK", "OK"))
})

test_that("check_sign_warnings errors on mismatched lengths", {
  expect_error(
    check_sign_warnings(
      coef_names = c("x1", "x2"),
      coef_values = c(1),
      expected_signs = c(x1 = "positive")
    ),
    "same length"
  )
})

# --- build_coef_table ---
test_that("build_coef_table works with cv.glmnet", {
  set.seed(42)
  x <- matrix(rnorm(100 * 5), 100, 5,
              dimnames = list(NULL, paste0("V", 1:5)))
  y <- x[, 1] * 3 + rnorm(100)
  fit <- glmnet::cv.glmnet(x, y)

  result <- build_coef_table(fit)
  expect_true(is.data.frame(result))
  expect_true("variable" %in% names(result))
  expect_true("coefficient" %in% names(result))
  expect_true(nrow(result) > 0)
  # Intercept should be present
  expect_true("(Intercept)" %in% result$variable)
})

test_that("build_coef_table works with explicit lambda", {
  set.seed(42)
  x <- matrix(rnorm(100 * 5), 100, 5,
              dimnames = list(NULL, paste0("V", 1:5)))
  y <- x[, 1] * 3 + rnorm(100)
  fit <- glmnet::cv.glmnet(x, y)

  result <- build_coef_table(fit, lambda = fit$lambda.min)
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
})

test_that("build_coef_table filters zero coefficients", {
  set.seed(42)
  x <- matrix(rnorm(100 * 5), 100, 5,
              dimnames = list(NULL, paste0("V", 1:5)))
  y <- x[, 1] * 3 + rnorm(100)
  fit <- glmnet::cv.glmnet(x, y)

  result <- build_coef_table(fit, lambda = fit$lambda.1se)
  # All coefficients in the table should be non-zero
  expect_true(all(result$coefficient != 0))
})

test_that("build_coef_table errors without lambda for non-cv model", {
  set.seed(42)
  x <- matrix(rnorm(100 * 5), 100, 5,
              dimnames = list(NULL, paste0("V", 1:5)))
  y <- rnorm(100)
  fit <- glmnet::glmnet(x, y)

  expect_error(build_coef_table(fit), "lambda must be specified")
})

test_that("build_coef_table works with non-cv model and lambda", {
  set.seed(42)
  x <- matrix(rnorm(100 * 5), 100, 5,
              dimnames = list(NULL, paste0("V", 1:5)))
  y <- x[, 1] * 3 + rnorm(100)
  fit <- glmnet::glmnet(x, y)

  result <- build_coef_table(fit, lambda = 0.1)
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
})

test_that("build_coef_table works with relaxed cv.glmnet", {
  set.seed(42)
  x <- matrix(rnorm(100 * 5), 100, 5,
              dimnames = list(NULL, paste0("V", 1:5)))
  y <- x[, 1] * 3 + x[, 2] * (-2) + rnorm(100)
  fit <- glmnet::cv.glmnet(x, y, relax = TRUE)

  gamma_val <- fit$relaxed$gamma.1se
  lambda_val <- fit$relaxed$lambda.1se

  result <- build_coef_table(fit, lambda = lambda_val, gamma = gamma_val)
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
})

# --- glmnet with coefficient bounds ---
test_that("glmnet respects upper/lower limits", {
  set.seed(42)
  x <- matrix(rnorm(200 * 3), 200, 3,
              dimnames = list(NULL, c("sqft", "age", "rooms")))
  y <- x[, 1] * 3 - x[, 2] * 2 + x[, 3] * 1 + rnorm(200)

  # Force sqft >= 0
  fit <- glmnet::cv.glmnet(x, y,
                           lower.limits = c(0, -Inf, -Inf),
                           upper.limits = c(Inf, Inf, Inf))
  coefs <- as.numeric(stats::coef(fit, s = fit$lambda.min))
  # Intercept is coefs[1], sqft is coefs[2]
  expect_true(coefs[2] >= 0)
})

# --- glmnet with penalty factors ---
test_that("glmnet respects penalty.factor = 0 (force in)", {
  set.seed(42)
  x <- matrix(rnorm(100 * 5), 100, 5,
              dimnames = list(NULL, paste0("V", 1:5)))
  y <- x[, 1] * 3 + rnorm(100, sd = 0.1)

  # Force V5 in (even though it has no real effect)
  pf <- c(1, 1, 1, 1, 0)
  fit <- glmnet::cv.glmnet(x, y, penalty.factor = pf)
  coefs <- stats::coef(fit, s = fit$lambda.min)
  # V5 should be non-zero since penalty = 0
  expect_true(as.numeric(coefs["V5", ]) != 0)
})

# --- glmnet with weights ---
test_that("glmnet accepts observation weights", {
  set.seed(42)
  x <- matrix(rnorm(100 * 3), 100, 3,
              dimnames = list(NULL, paste0("V", 1:3)))
  y <- x[, 1] * 3 + rnorm(100)
  w <- rep(1, 100)
  w[1:10] <- 0.1  # downweight first 10

  fit <- glmnet::cv.glmnet(x, y, weights = w)
  expect_s3_class(fit, "cv.glmnet")
  result <- build_coef_table(fit)
  expect_true(nrow(result) > 0)
})

# --- glmnet with interactions ---
test_that("model.matrix correctly creates interaction terms", {
  df <- data.frame(
    sqft = c(1000, 1500, 2000, 2500),
    age = c(10, 20, 30, 40),
    quality = factor(c("A", "B", "A", "B"))
  )
  formula <- stats::as.formula("~ sqft + age + quality + sqft:age - 1")
  x <- stats::model.matrix(formula, data = df)

  expect_true("sqft:age" %in% colnames(x))
  expect_true("sqft" %in% colnames(x))
  expect_true("age" %in% colnames(x))
  # Factor creates dummy columns
  expect_true(any(grepl("quality", colnames(x))))
})

# --- glmnet relaxed lasso ---
test_that("relaxed cv.glmnet has expected structure", {
  set.seed(42)
  x <- matrix(rnorm(200 * 5), 200, 5,
              dimnames = list(NULL, paste0("V", 1:5)))
  y <- x[, 1] * 3 + x[, 2] * (-2) + rnorm(200)
  fit <- glmnet::cv.glmnet(x, y, relax = TRUE)

  expect_true(inherits(fit, "cv.relaxed"))
  expect_true(inherits(fit, "cv.glmnet"))
  expect_true(!is.null(fit$relaxed))
  expect_true(!is.null(fit$relaxed$gamma.1se))
  expect_true(!is.null(fit$relaxed$lambda.1se))
})

test_that("predict works with relaxed fit and gamma", {
  set.seed(42)
  x <- matrix(rnorm(200 * 5), 200, 5,
              dimnames = list(NULL, paste0("V", 1:5)))
  y <- x[, 1] * 3 + x[, 2] * (-2) + rnorm(200)
  fit <- glmnet::cv.glmnet(x, y, relax = TRUE)

  gamma_val <- fit$relaxed$gamma.1se
  lambda_val <- fit$relaxed$lambda.1se
  preds <- stats::predict(fit, newx = x, s = lambda_val,
                          gamma = gamma_val, type = "response")
  expect_equal(nrow(preds), nrow(x))
})

# --- Alpha grid search ---
test_that("grid search over alpha finds reasonable fit", {
  set.seed(42)
  x <- matrix(rnorm(200 * 5), 200, 5,
              dimnames = list(NULL, paste0("V", 1:5)))
  y <- x[, 1] * 3 + x[, 2] * (-2) + rnorm(200)

  alphas <- seq(0, 1, length.out = 5)
  best_cvm <- Inf
  best_alpha <- NULL
  for (a in alphas) {
    fit <- glmnet::cv.glmnet(x, y, alpha = a, nfolds = 5)
    if (min(fit$cvm) < best_cvm) {
      best_cvm <- min(fit$cvm)
      best_alpha <- a
    }
  }
  expect_true(!is.null(best_alpha))
  expect_true(best_alpha >= 0 && best_alpha <= 1)
})

# --- Module UI functions ---
test_that("module UI functions return shiny tags", {
  expect_s3_class(dataImportUI("test"), "shiny.tag.list")
  expect_s3_class(variableConfigUI("test"), "shiny.tag.list")
  expect_s3_class(dataPreviewUI("test"), "shiny.tag.list")
  expect_s3_class(modelingUI("test"), "shiny.tag.list")
  expect_s3_class(fitModelUI("test"), "shiny.tag.list")
  expect_s3_class(coefficientsUI("test"), "shiny.tag.list")
  expect_s3_class(diagnosticsUI("test"), "shiny.tag.list")
  expect_s3_class(reportUI("test"), "shiny.tag.list")
})

# --- Module server integration tests ---
test_that("coefficientsServer returns coef_df reactive", {
  set.seed(42)
  x <- matrix(rnorm(200 * 5), 200, 5,
              dimnames = list(NULL, paste0("V", 1:5)))
  y <- x[, 1] * 3 + x[, 2] * (-2) + rnorm(200)
  fit <- glmnet::cv.glmnet(x, y)

  model_module <- list(
    model = shiny::reactiveVal(fit),
    lambda = shiny::reactiveVal(fit$lambda.1se),
    gamma = shiny::reactiveVal(NULL),
    x_matrix = shiny::reactiveVal(x),
    y_vector = shiny::reactiveVal(y),
    family = shiny::reactiveVal("gaussian"),
    fitted = shiny::reactiveVal(TRUE),
    fit_count = shiny::reactiveVal(1L)
  )

  data_module <- list(
    expected_signs = shiny::reactiveVal(
      c(V1 = "positive", V2 = "negative")
    )
  )

  shiny::testServer(
    coefficientsServer,
    args = list(model_module = model_module, data_module = data_module),
    {
      result <- session$returned$coef_df()
      expect_true(is.data.frame(result))
      expect_true(nrow(result) > 0)
      expect_true("sign_warning" %in% names(result))
      expect_true("(Intercept)" %in% result$variable)
    }
  )
})

test_that("coefficientsServer flags sign violations", {
  set.seed(42)
  x <- matrix(rnorm(200 * 3), 200, 3,
              dimnames = list(NULL, c("sqft", "age", "rooms")))
  y <- x[, 1] * 3 - x[, 2] * 2 + x[, 3] * 1 + rnorm(200)
  fit <- glmnet::cv.glmnet(x, y)

  model_module <- list(
    model = shiny::reactiveVal(fit),
    lambda = shiny::reactiveVal(fit$lambda.min),
    gamma = shiny::reactiveVal(NULL),
    x_matrix = shiny::reactiveVal(x),
    y_vector = shiny::reactiveVal(y),
    family = shiny::reactiveVal("gaussian"),
    fitted = shiny::reactiveVal(TRUE),
    fit_count = shiny::reactiveVal(1L)
  )

  # Deliberately set wrong expected sign for sqft (expect negative, but it's positive)
  data_module <- list(
    expected_signs = shiny::reactiveVal(
      c(sqft = "negative", age = "positive", rooms = "either")
    )
  )

  shiny::testServer(
    coefficientsServer,
    args = list(model_module = model_module, data_module = data_module),
    {
      result <- session$returned$coef_df()
      sqft_row <- result[result$variable == "sqft", ]
      if (nrow(sqft_row) > 0 && sqft_row$coefficient > 0) {
        expect_match(sqft_row$sign_warning, "Expected negative")
      }
      age_row <- result[result$variable == "age", ]
      if (nrow(age_row) > 0 && age_row$coefficient < 0) {
        expect_match(age_row$sign_warning, "Expected positive")
      }
    }
  )
})

test_that("coefficientsServer handles relaxed fits", {
  set.seed(42)
  x <- matrix(rnorm(200 * 5), 200, 5,
              dimnames = list(NULL, paste0("V", 1:5)))
  y <- x[, 1] * 3 + x[, 2] * (-2) + rnorm(200)
  fit <- glmnet::cv.glmnet(x, y, relax = TRUE)

  gamma_val <- fit$relaxed$gamma.1se
  lambda_val <- fit$relaxed$lambda.1se

  model_module <- list(
    model = shiny::reactiveVal(fit),
    lambda = shiny::reactiveVal(lambda_val),
    gamma = shiny::reactiveVal(gamma_val),
    x_matrix = shiny::reactiveVal(x),
    y_vector = shiny::reactiveVal(y),
    family = shiny::reactiveVal("gaussian"),
    fitted = shiny::reactiveVal(TRUE),
    fit_count = shiny::reactiveVal(1L)
  )

  data_module <- list(
    expected_signs = shiny::reactiveVal(
      stats::setNames(character(0), character(0))
    )
  )

  shiny::testServer(
    coefficientsServer,
    args = list(model_module = model_module, data_module = data_module),
    {
      result <- session$returned$coef_df()
      expect_true(is.data.frame(result))
      expect_true(nrow(result) > 0)
    }
  )
})

# --- Combined features test ---
test_that("all glmnet features work together", {
  set.seed(42)
  n <- 150
  x <- matrix(rnorm(n * 4), n, 4,
              dimnames = list(NULL, c("sqft", "age", "rooms", "noise")))
  y <- x[, 1] * 3 - x[, 2] * 2 + x[, 3] * 1 + rnorm(n)
  w <- c(0, rep(1, n - 1))  # first row excluded

  # Filter weight = 0
  keep <- w != 0
  x_fit <- x[keep, , drop = FALSE]
  y_fit <- y[keep]
  w_fit <- w[keep]

  fit <- glmnet::cv.glmnet(
    x = x_fit, y = y_fit,
    alpha = 0.5,
    weights = w_fit,
    penalty.factor = c(0, 1, 1, 1),  # force sqft in
    lower.limits = c(0, -Inf, -Inf, -Inf),
    upper.limits = c(Inf, 0, Inf, Inf),
    nfolds = 5,
    relax = TRUE
  )

  expect_true(inherits(fit, "cv.relaxed"))

  gamma_val <- fit$relaxed$gamma.1se
  lambda_val <- fit$relaxed$lambda.1se

  result <- build_coef_table(fit, lambda = lambda_val, gamma = gamma_val)
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)

  # sqft should be forced in and positive
  sqft_row <- result[result$variable == "sqft", ]
  if (nrow(sqft_row) > 0) {
    expect_true(sqft_row$coefficient >= 0)
  }

  # age should be non-positive (if present)
  age_row <- result[result$variable == "age", ]
  if (nrow(age_row) > 0) {
    expect_true(age_row$coefficient <= 0)
  }

  # Predictions work
  preds <- stats::predict(fit, newx = x_fit, s = lambda_val,
                          gamma = gamma_val, type = "response")
  expect_equal(nrow(preds), nrow(x_fit))
})
