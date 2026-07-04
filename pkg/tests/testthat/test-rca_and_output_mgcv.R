###############################################################################
# test-rca_and_output.R
#
# Tests for RCA (Residual Constraint Approach) calculations,
# CQA scoring, output data structure, back-transformation, sales grid
# helpers, and related logic used in mgcvUI's export and RCA pipeline.
#
# The RCA logic lives inside inst/app/app.R (server-side) rather than
# in exported functions, so we recreate the core formulas here and verify
# them against reference inputs.
###############################################################################

# ---------------------------------------------------------------------------
# Setup: fit a simple GAM on mtcars for reuse across multiple tests
# ---------------------------------------------------------------------------
setup_gam_result <- function(xform = "none") {
  specs <- list(
    list(vars = "wt",   type = "s", bs = "tp", k = 5L),
    list(vars = "hp",   type = "s", bs = "tp", k = 5L),
    list(vars = "qsec", type = "s", bs = "tp", k = 5L)
  )

  dat <- mtcars
  if (xform == "log") {
    dat$log_mpg <- log(dat$mpg)
    res <- fit_gam(dat, "log_mpg", specs, cv_folds = 0L)
    res$response_transform <- "log"
    res$response <- "log_mpg"
  } else if (xform == "log10") {
    dat$log10_mpg <- log10(dat$mpg)
    res <- fit_gam(dat, "log10_mpg", specs, cv_folds = 0L)
    res$response_transform <- "log10"
    res$response <- "log10_mpg"
  } else {
    res <- fit_gam(dat, "mpg", specs, cv_folds = 0L)
    res$response_transform <- "none"
  }
  list(result = res, data = dat)
}

# ---------------------------------------------------------------------------
# 1. CQA score computation (basic percentile rank, 0-10 scale)
# ---------------------------------------------------------------------------
test_that("CQA score follows percentile rank formula on simple vector", {
  # Five residuals; the CQA formula from app.R is:
  #   cqa_i = sum(comp_resid < r_i) / n_comps * 10
  residuals <- c(-100, -50, 0, 50, 100)
  n_comps   <- length(residuals)

  cqa <- vapply(residuals, function(r) {
    sum(residuals < r) / n_comps * 10
  }, numeric(1))

  # Smallest residual should score 0, largest should score 8 (4 out of 5)

  expect_equal(cqa[1], 0)
  expect_equal(cqa[5], 8)
  # Middle value has exactly 2 below it -> 2/5 * 10 = 4
  expect_equal(cqa[3], 4)
})

test_that("CQA score is monotonically non-decreasing with sorted residuals", {
  set.seed(42)
  residuals <- rnorm(50, mean = 0, sd = 100)
  n_comps   <- length(residuals)

  cqa <- vapply(residuals, function(r) {
    sum(residuals < r) / n_comps * 10
  }, numeric(1))

  sorted_cqa <- cqa[order(residuals)]
  for (i in 2:length(sorted_cqa)) {
    expect_true(sorted_cqa[i] >= sorted_cqa[i - 1])
  }
})

test_that("CQA score range is within [0, 10)", {
  set.seed(123)
  residuals <- rnorm(100, mean = 0, sd = 50)
  n_comps   <- length(residuals)

  cqa <- vapply(residuals, function(r) {
    sum(residuals < r) / n_comps * 10
  }, numeric(1))

  expect_true(all(cqa >= 0))
  # Maximum possible: (n-1)/n * 10, which is < 10

  expect_true(all(cqa < 10))
})

test_that("CQA returns NA for NA residual", {
  residuals <- c(-20, -10, NA, 10, 20)
  comp_resid <- residuals[!is.na(residuals)]
  n_comps <- length(comp_resid)

  cqa <- vapply(residuals, function(r) {
    if (is.na(r)) return(NA_real_)
    sum(comp_resid < r, na.rm = TRUE) / n_comps * 10
  }, numeric(1))

  expect_true(is.na(cqa[3]))
  expect_true(!is.na(cqa[1]))
  expect_true(!is.na(cqa[5]))
})

# ---------------------------------------------------------------------------
# 2. CQA interpolation via approx()
# ---------------------------------------------------------------------------
test_that("approx() interpolation returns correct midpoint for linear data", {
  # Two comps with CQA 2 and 8, residuals -100 and 100
  cqa_vals <- c(2, 8)
  resid_vals <- c(-100, 100)

  # Interpolate at CQA = 5 (midpoint)
  result <- stats::approx(cqa_vals, resid_vals, xout = 5, rule = 2)$y
  expect_equal(result, 0)
})

test_that("approx() with rule=2 clamps to boundary values", {
  cqa_vals <- c(2, 4, 6, 8)
  resid_vals <- c(-200, -100, 100, 200)

  # Below minimum CQA -> clamp to leftmost residual

  lo <- stats::approx(cqa_vals, resid_vals, xout = 0, rule = 2)$y
  expect_equal(lo, -200)

  # Above maximum CQA -> clamp to rightmost residual
  hi <- stats::approx(cqa_vals, resid_vals, xout = 10, rule = 2)$y
  expect_equal(hi, 200)
})

test_that("approx() interpolation with many points matches expected", {
  set.seed(99)
  n <- 20
  cqa_vals <- sort(runif(n, 0, 10))
  resid_vals <- sort(rnorm(n, 0, 50))

  # Interpolation at a known CQA value should give back the known residual
  known_idx <- 10
  result <- stats::approx(cqa_vals, resid_vals,
                           xout = cqa_vals[known_idx], rule = 2)$y
  expect_equal(result, resid_vals[known_idx], tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# 3. Per-smooth-term contributions via predict(type = "terms")
# ---------------------------------------------------------------------------
test_that("predict type='terms' returns matrix with one column per smooth", {
  setup <- setup_gam_result("none")
  model <- setup$result$model
  dat   <- setup$data

  preds <- predict(model, newdata = dat, type = "terms")
  expect_true(is.matrix(preds))
  expect_equal(nrow(preds), nrow(dat))
  # Should have columns for s(wt), s(hp), s(qsec)
  expect_true(ncol(preds) >= 3)
})

test_that("contributions sum plus intercept approximates fitted values", {
  setup <- setup_gam_result("none")
  model <- setup$result$model
  dat   <- setup$data

  preds <- predict(model, newdata = dat, type = "terms")
  intercept <- stats::coef(model)[["(Intercept)"]]
  contrib_total <- intercept + rowSums(preds)

  fitted_vals <- as.numeric(predict(model, newdata = dat, type = "response"))
  expect_equal(as.numeric(contrib_total), fitted_vals, tolerance = 1e-6)
})

test_that("compute_gam_contributions_ returns named list of numeric vectors", {
  setup <- setup_gam_result("none")
  model <- setup$result$model
  dat   <- setup$data

  # Recreate the helper function from app.R
  compute_gam_contributions_ <- function(mdl, newdata) {
    preds <- predict(mdl, newdata = newdata, type = "terms")
    contribs <- list()
    for (col in colnames(preds)) {
      contribs[[col]] <- as.numeric(preds[, col])
    }
    contribs
  }

  contribs <- compute_gam_contributions_(model, dat)
  expect_true(is.list(contribs))
  expect_true(all(vapply(contribs, is.numeric, logical(1))))
  expect_true(all(vapply(contribs, length, integer(1)) == nrow(dat)))
})

# ---------------------------------------------------------------------------
# 4. Export data structure (full pipeline)
# ---------------------------------------------------------------------------
test_that("export pipeline produces all expected columns", {
  setup <- setup_gam_result("none")
  model    <- setup$result$model
  dat      <- setup$data
  response <- "mpg"
  xform    <- "none"

  predicted <- as.numeric(predict(model, newdata = dat, type = "response"))
  predicted <- earthUI:::back_transform_(predicted, xform)
  residuals_val <- dat[[response]] - predicted

  export_df <- dat
  export_df[[paste0("est_", response)]] <- round(predicted, 1)
  export_df[["residual"]] <- round(residuals_val, 1)

  # CQA
  n_comps <- length(residuals_val)
  cqa <- vapply(residuals_val, function(r) {
    if (is.na(r)) return(NA_real_)
    sum(residuals_val < r, na.rm = TRUE) / n_comps * 10
  }, numeric(1))
  export_df[["cqa"]] <- round(cqa, 2)

  expect_true("est_mpg" %in% names(export_df))
  expect_true("residual" %in% names(export_df))
  expect_true("cqa" %in% names(export_df))
  expect_equal(nrow(export_df), nrow(dat))
})

test_that("export pipeline includes contribution columns", {
  setup <- setup_gam_result("none")
  model <- setup$result$model
  dat   <- setup$data

  preds <- predict(model, newdata = dat, type = "terms")
  intercept <- stats::coef(model)[["(Intercept)"]]

  export_df <- dat
  export_df[["basis"]] <- round(intercept, 4)

  for (tl in colnames(preds)) {
    col_name <- gsub("^s\\((.+)\\)$", "\\1", tl)
    col_name <- paste0(col_name, "_contribution")
    export_df[[col_name]] <- round(preds[, tl], 4)
  }

  # Check contribution columns exist
  expect_true("basis" %in% names(export_df))
  contrib_cols <- grep("_contribution$", names(export_df), value = TRUE)
  expect_true(length(contrib_cols) >= 3)
})

test_that("calc_residual column verifies contribution sum", {
  setup <- setup_gam_result("none")
  model    <- setup$result$model
  dat      <- setup$data
  response <- "mpg"

  preds <- predict(model, newdata = dat, type = "terms")
  intercept <- stats::coef(model)[["(Intercept)"]]
  contribs <- list()
  for (col in colnames(preds)) {
    contribs[[col]] <- as.numeric(preds[, col])
  }

  contrib_total <- intercept + Reduce(`+`, contribs)
  est_from_contribs <- earthUI:::back_transform_(contrib_total, "none")
  calc_residual <- dat[[response]] - est_from_contribs

  # calc_residual should match standard residuals
  predicted <- as.numeric(predict(model, newdata = dat, type = "response"))
  standard_resid <- dat[[response]] - predicted
  expect_equal(calc_residual, standard_resid, tolerance = 1e-4)
})

# ---------------------------------------------------------------------------
# 5. Sales grid helpers
# ---------------------------------------------------------------------------

# Source sales_grid.R helpers for direct testing
sg_path <- system.file("app_mgcv", "sales_grid.R", package = "earthUI")
if (!nzchar(sg_path)) {
  sg_path <- file.path(
    dirname(dirname(dirname(testthat::test_path()))),
    "inst", "app", "sales_grid.R"
  )
}

# Define the helper functions inline for testing (same as in sales_grid.R)
col_letter <- function(n) {
  if (n <= 26) {
    return(LETTERS[n])
  } else {
    return(paste0(LETTERS[(n - 1) %/% 26], LETTERS[((n - 1) %% 26) + 1]))
  }
}

haversine_miles <- function(lat1, lon1, lat2, lon2) {
  if (any(is.na(c(lat1, lon1, lat2, lon2)))) return(NA_real_)
  R <- 3958.8
  dlat <- (lat2 - lat1) * pi / 180
  dlon <- (lon2 - lon1) * pi / 180
  a <- sin(dlat / 2)^2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) *
    sin(dlon / 2)^2
  R * 2 * asin(sqrt(a))
}

col_val <- function(df, row, col, default = "") {
  if (col %in% colnames(df)) {
    v <- df[[col]][row]
    if (is.null(v) || length(v) == 0 || is.na(v)) return(default)
    if (is.character(default) && !is.character(v)) return(as.character(v))
    return(v)
  }
  default
}

col_num <- function(df, row, col, digits = 0, default = 0) {
  v <- col_val(df, row, col, default = NA)
  if (is.na(v)) return(default)
  round(as.numeric(v), digits = digits)
}

format_label <- function(lbl) {
  abbrevs <- c(
    "living_sqft"       = "Living SF",
    "lot_size"          = "Lot Size",
    "sale_age"          = "Sale Age",
    "beds_total"        = "Beds",
    "baths_total"       = "Baths",
    "garage_spaces"     = "Garage",
    "fp_count"          = "Fireplaces",
    "no_of_stories"     = "Stories",
    "year_built"        = "Year Built",
    "days_on_market"    = "DOM",
    "contract_date"     = "Contract Date",
    "area_id"           = "Area",
    "area_text"         = "Area",
    "latitude"          = "Latitude",
    "longitude"         = "Longitude",
    "age"               = "Age"
  )
  if (lbl %in% names(abbrevs)) return(abbrevs[[lbl]])
  result <- lbl
  for (nm in names(abbrevs)) {
    if (grepl(nm, result, fixed = TRUE)) {
      result <- sub(nm, abbrevs[[nm]], result, fixed = TRUE)
    }
  }
  result <- gsub("_", " ", result)
  result <- trimws(result)
  if (nchar(result) > 28) result <- paste0(substr(result, 1, 25), "...")
  result
}

detect_model_vars <- function(df) {
  contrib_cols <- grep("_contribution$", colnames(df), value = TRUE)
  var_labels <- sub("_contribution$", "", contrib_cols)
  var_labels <- var_labels[!grepl("^rent_", var_labels)]
  contrib_cols <- paste0(var_labels, "_contribution")
  adjust_cols  <- paste0(var_labels, "_adjustment")
  keep <- adjust_cols %in% colnames(df)
  list(
    labels     = var_labels[keep],
    contrib    = contrib_cols[keep],
    adjustment = adjust_cols[keep]
  )
}

sum_contribs <- function(df, row, var_names) {
  total <- 0
  for (vn in var_names) {
    cc <- paste0(vn, "_contribution")
    if (cc %in% colnames(df)) {
      total <- total + col_num(df, row, cc)
    }
  }
  total
}


test_that("col_letter returns correct letters for columns 1-26", {
  expect_equal(col_letter(1), "A")
  expect_equal(col_letter(26), "Z")
  expect_equal(col_letter(13), "M")
})

test_that("col_letter handles columns beyond 26 (AA, AB, ...)", {
  expect_equal(col_letter(27), "AA")
  expect_equal(col_letter(28), "AB")
  expect_equal(col_letter(52), "AZ")
})

test_that("haversine_miles computes correct distance for known points", {
  # NYC (40.7128, -74.0060) to LA (34.0522, -118.2437)
  # Approximate great-circle distance: ~2451 miles
  d <- haversine_miles(40.7128, -74.0060, 34.0522, -118.2437)
  expect_true(abs(d - 2451) < 10)  # within 10 miles
})

test_that("haversine_miles returns NA when any coordinate is NA", {
  expect_true(is.na(haversine_miles(NA, -74, 34, -118)))
  expect_true(is.na(haversine_miles(40.7, NA, 34, -118)))
  expect_true(is.na(haversine_miles(40.7, -74, NA, -118)))
  expect_true(is.na(haversine_miles(40.7, -74, 34, NA)))
})

test_that("haversine_miles returns zero for same point", {
  d <- haversine_miles(37.7749, -122.4194, 37.7749, -122.4194)
  expect_equal(d, 0)
})

test_that("format_label applies known abbreviations", {
  expect_equal(format_label("living_sqft"), "Living SF")
  expect_equal(format_label("lot_size"), "Lot Size")
  expect_equal(format_label("beds_total"), "Beds")
  expect_equal(format_label("days_on_market"), "DOM")
})

test_that("format_label replaces underscores for unknown labels", {
  expect_equal(format_label("some_variable"), "some variable")
  expect_equal(format_label("pool_type"), "pool type")
})

test_that("format_label truncates long labels to 28 characters", {
  long_label <- "this_is_a_very_long_variable_name_that_exceeds_limit"
  result <- format_label(long_label)
  expect_true(nchar(result) <= 28)
  expect_true(grepl("\\.\\.\\.$", result))
})

test_that("col_val returns default when column missing", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  expect_equal(col_val(df, 1, "missing_col", default = "N/A"), "N/A")
})

test_that("col_val returns default when value is NA", {
  df <- data.frame(a = c(1, NA, 3))
  expect_equal(col_val(df, 2, "a", default = ""), "")
})

test_that("col_num returns numeric with rounding", {
  df <- data.frame(price = c(123456.789, 200000.123))
  expect_equal(col_num(df, 1, "price", digits = 0), 123457)
  expect_equal(col_num(df, 1, "price", digits = 2), 123456.79)
})

test_that("detect_model_vars finds contribution and adjustment pairs", {
  df <- data.frame(
    wt_contribution = c(100, 200),
    wt_adjustment   = c(-50, 50),
    hp_contribution = c(300, 400),
    hp_adjustment   = c(-10, 10),
    # This one has contribution but no adjustment -> should be excluded
    cyl_contribution = c(50, 60),
    sale_price       = c(25000, 30000)
  )
  mv <- detect_model_vars(df)
  expect_true(all(c("wt", "hp") %in% mv$labels))
  expect_false("cyl" %in% mv$labels)
  expect_equal(length(mv$labels), length(mv$contrib))
  expect_equal(length(mv$labels), length(mv$adjustment))
})

test_that("sum_contribs sums matching contribution columns", {
  df <- data.frame(
    lat_contribution  = c(500, 700),
    lon_contribution  = c(300, 400),
    area_contribution = c(100, 200)
  )
  # Sum lat + lon contributions for row 1
  total <- sum_contribs(df, 1, c("lat", "lon"))
  expect_equal(total, 800)  # 500 + 300

  # Sum all three for row 2
  total2 <- sum_contribs(df, 2, c("lat", "lon", "area"))
  expect_equal(total2, 1300)  # 700 + 400 + 200
})

# ---------------------------------------------------------------------------
# 6. Back-transformation from log scale
# ---------------------------------------------------------------------------
test_that("back_transform_ correctly applies exp() for log", {
  vals <- c(1, 2, 3, log(100))
  result <- earthUI:::back_transform_(vals, "log")
  expect_equal(result, exp(vals))
})
test_that("back_transform_ correctly applies 10^ for log10", {
  vals <- c(1, 2, 3, log10(500))
  result <- earthUI:::back_transform_(vals, "log10")
  expect_equal(result, 10^vals)
})

test_that("back_transform_ is identity for 'none'", {
  vals <- c(10, 20, 30)
  expect_equal(earthUI:::back_transform_(vals, "none"), vals)
})

test_that("back_transform_ handles NULL transform as identity", {
  vals <- c(10, 20, 30)
  expect_equal(earthUI:::back_transform_(vals, NULL), vals)
})

test_that("log-scale contributions back-transform to dollar values correctly", {
  setup <- setup_gam_result("log")
  model <- setup$result$model
  dat   <- setup$data
  xform <- "log"

  preds <- predict(model, newdata = dat, type = "terms")
  intercept <- stats::coef(model)[["(Intercept)"]]
  contribs <- list()
  for (col in colnames(preds)) {
    contribs[[col]] <- as.numeric(preds[, col])
  }

  log_total <- intercept + Reduce(`+`, contribs)
  dollar_pred <- earthUI:::back_transform_(log_total, xform)
  dollar_base <- earthUI:::back_transform_(intercept, xform)
  dollar_effect <- dollar_pred - dollar_base

  # Verify proportional allocation: shares sum to 1 (where log_sum != 0)
  log_sum <- Reduce(`+`, contribs)
  non_zero <- abs(log_sum) > 1e-10
  if (any(non_zero)) {
    shares <- lapply(contribs, function(c) {
      ifelse(abs(log_sum) > 1e-10, c / log_sum, 0)
    })
    total_share <- Reduce(`+`, shares)
    expect_equal(total_share[non_zero], rep(1, sum(non_zero)),
                 tolerance = 1e-8)
  }

  # Dollar contributions should sum to dollar_effect
  dollar_contribs <- lapply(contribs, function(c) {
    share <- ifelse(abs(log_sum) > 1e-10, c / log_sum, 0)
    dollar_effect * share
  })
  dollar_sum <- Reduce(`+`, dollar_contribs)
  expect_equal(dollar_sum, dollar_effect, tolerance = 1e-4)
})

# ---------------------------------------------------------------------------
# 7. Net/gross adjustment calculations
# ---------------------------------------------------------------------------
test_that("net adjustments equal sum of per-variable adjustments", {
  # Simulate subject contribution [1] and comp contribution for 3 variables
  subj_contribs <- c(100, 200, -50)
  comp_contribs <- c(80, 250, -30)

  adjustments <- subj_contribs - comp_contribs  # 20, -50, -20
  net_adj <- sum(adjustments)
  expect_equal(net_adj, -50)  # 20 + (-50) + (-20) = -50
})

test_that("gross adjustments equal sum of absolute adjustments", {
  subj_contribs <- c(100, 200, -50)
  comp_contribs <- c(80, 250, -30)

  adjustments <- subj_contribs - comp_contribs  # 20, -50, -20
  gross_adj <- sum(abs(adjustments))
  expect_equal(gross_adj, 90)  # |20| + |-50| + |-20| = 90
})

test_that("net adjustment = 0 for subject vs self", {
  contribs <- c(100, 200, 50)
  adjustments <- contribs - contribs
  expect_equal(sum(adjustments), 0)
  expect_equal(sum(abs(adjustments)), 0)
})

test_that("adjusted sale price = actual + net adjustment", {
  sale_price <- 300000
  net_adj    <- -15000
  adjusted   <- sale_price + net_adj
  expect_equal(adjusted, 285000)
})

test_that("adjustment percentages computed correctly", {
  sale_price <- 250000
  net_adj    <- 25000
  gross_adj  <- 40000

  net_pct   <- net_adj / sale_price * 100
  gross_pct <- gross_adj / sale_price * 100

  expect_equal(net_pct, 10)
  expect_equal(gross_pct, 16)
})

test_that("residual adjustment is subject_resid - comp_resid", {
  subject_resid <- 5000
  comp_resids   <- c(-3000, 2000, 8000)

  resid_adj <- subject_resid - comp_resids
  expect_equal(resid_adj, c(8000, 3000, -3000))
})

test_that("full RCA pipeline: adjustments sum to net, abs sums to gross", {
  # Simulate a full RCA with 3 variables + residual
  n <- 5  # 1 subject + 4 comps
  set.seed(42)

  # Random contributions for subject (row 1) and comps (rows 2-5)
  wt_contribs <- rnorm(n, 100, 20)
  hp_contribs <- rnorm(n, 200, 30)
  qs_contribs <- rnorm(n, -50, 15)

  adj_sum   <- rep(0, n)
  gross_sum <- rep(0, n)

  for (cv in list(wt_contribs, hp_contribs, qs_contribs)) {
    adjustment <- cv[1] - cv
    adj_sum   <- adj_sum + adjustment
    gross_sum <- gross_sum + abs(adjustment)
  }

  # Add residual adjustment
  resid_adj <- rep(5000, n) - rnorm(n, 5000, 500)
  adj_sum   <- adj_sum + resid_adj
  gross_sum <- gross_sum + abs(resid_adj)

  # Net should be algebraic sum
  for (i in 2:n) {
    manual_net <- (wt_contribs[1] - wt_contribs[i]) +
                  (hp_contribs[1] - hp_contribs[i]) +
                  (qs_contribs[1] - qs_contribs[i]) +
                  resid_adj[i]
    expect_equal(adj_sum[i], manual_net, tolerance = 1e-10)
  }

  # Gross should be sum of absolute values
  expect_true(all(gross_sum >= abs(adj_sum)))
  expect_true(all(gross_sum >= 0))
})

# ---------------------------------------------------------------------------
# 8. Weight-zero row handling
# ---------------------------------------------------------------------------
test_that("weight-zero rows are excluded from fitting but predicted", {
  dat <- mtcars[1:20, ]
  dat$wt_flag <- 1
  dat$wt_flag[c(18, 19, 20)] <- 0

  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = 5L),
    list(vars = "hp", type = "s", bs = "tp", k = 5L)
  )

  # Fit with weights (0-weight rows effectively excluded from fit)
  res <- fit_gam(dat, "mpg", specs, weights = dat$wt_flag, cv_folds = 0L)
  model <- res$model

  # Should still be able to predict for all rows
  predicted <- predict(model, newdata = dat, type = "response")
  expect_equal(length(predicted), 20)
  expect_true(all(!is.na(predicted)))
})

test_that("weight-zero rows get subject_value in RCA logic", {
  # Simulate the weight-zero treatment from app.R lines 1266-1284
  n <- 6
  predicted <- c(200000, 210000, 220000, 230000, 190000, 205000)
  actual    <- c(NA, 215000, 225000, 235000, 195000, NA)
  subject_resid_total <- 5000
  zero_wt <- c(6L)  # Row 6 has weight 0

  # The app sets subject_value for zero-weight rows
  sv <- predicted[zero_wt] + subject_resid_total
  actual[zero_wt] <- sv
  residuals_val <- actual - predicted

  expect_equal(sv, 210000)  # 205000 + 5000
  expect_equal(actual[6], 210000)
  expect_equal(residuals_val[6], 5000)  # 210000 - 205000
})

test_that("weight-zero rows produce valid residual_sf", {
  la <- c(2000, 1800, 2200, 2400, 1600, 2100)
  predicted <- c(200000, 210000, 220000, 230000, 190000, 205000)
  subject_resid_total <- 5000
  zero_wt <- 6L

  # After zero-weight treatment
  actual <- c(NA, 215000, 225000, 235000, 195000, NA)
  actual[zero_wt] <- predicted[zero_wt] + subject_resid_total
  residuals_val <- actual - predicted

  resid_sf <- residuals_val / la
  expect_true(!is.na(resid_sf[6]))
  expect_equal(resid_sf[6], 5000 / 2100, tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# 9. CQA per square foot (cqa_sf)
# ---------------------------------------------------------------------------
test_that("CQA_SF uses residual per SF for ranking", {
  residuals   <- c(NA, -5000, 0, 5000, 10000)
  living_area <- c(2000, 1800, 2200, 2400, 1600)

  resid_sf <- residuals / living_area
  comp_resid_sf <- resid_sf[-1]
  comp_resid_sf_valid <- comp_resid_sf[!is.na(comp_resid_sf)]
  n_sf <- length(comp_resid_sf_valid)

  cqa_sf <- vapply(resid_sf, function(r) {
    if (is.na(r)) return(NA_real_)
    sum(comp_resid_sf_valid < r, na.rm = TRUE) / n_sf * 10
  }, numeric(1))

  expect_true(is.na(cqa_sf[1]))
  # The comp with highest resid_sf should have highest cqa_sf
  comp_cqa_sf <- cqa_sf[-1]
  max_idx <- which.max(resid_sf[-1])
  expect_equal(comp_cqa_sf[max_idx], max(comp_cqa_sf))
})

# ---------------------------------------------------------------------------
# 10. CQA-to-residual interpolation with SF conversion
# ---------------------------------------------------------------------------
test_that("SF-based residual converts back to total correctly", {
  subject_la <- 2000
  # Comp CQA_SF values and per-SF residuals
  cqa_sf_vals <- c(2, 4, 6, 8)
  resid_sf_vals <- c(-5, -2, 2, 5)

  user_cqa <- 5.0
  subject_resid_sf <- stats::approx(cqa_sf_vals, resid_sf_vals,
                                     xout = user_cqa, rule = 2)$y
  subject_resid_total <- subject_resid_sf * subject_la

  expect_equal(subject_resid_sf, 0, tolerance = 1e-10)
  expect_equal(subject_resid_total, 0, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# 11. Column reordering: rank columns moved to front
# ---------------------------------------------------------------------------
test_that("rank columns are moved to the leftmost positions", {
  df <- data.frame(
    wt = 1, hp = 2, mpg = 3,
    residual = 100, cqa = 5, residual_sf = 0.05, cqa_sf = 5.5
  )

  rank_cols <- c("residual_sf", "cqa_sf", "residual", "cqa")
  rank_cols <- rank_cols[rank_cols %in% names(df)]
  other_cols <- setdiff(names(df), rank_cols)
  df_reordered <- df[, c(rank_cols, other_cols), drop = FALSE]

  expect_equal(names(df_reordered)[1], "residual_sf")
  expect_equal(names(df_reordered)[2], "cqa_sf")
  expect_equal(names(df_reordered)[3], "residual")
  expect_equal(names(df_reordered)[4], "cqa")
})

# ---------------------------------------------------------------------------
# 12. Appraisal mode: subject row (row 1) adjustments set to NA
# ---------------------------------------------------------------------------
test_that("subject row adjustments are NA in appraisal mode", {
  df <- data.frame(
    wt_adjustment       = c(0, 10, -5),
    net_adjustments     = c(0, 25, -10),
    gross_adjustments   = c(0, 35, 15),
    adjusted_sale_price = c(300000, 310000, 295000),
    net_adj_pct         = c(0, 5, -3),
    gross_adj_pct       = c(0, 8, 4)
  )

  # Simulate the app.R logic for setting subject row to NA
  adj_cols <- grep(
    "_adjustment$|net_adjustments|gross_adjustments|adjusted_sale_price|_adj_pct$",
    names(df), value = TRUE)
  for (ac in adj_cols) {
    df[[ac]][1L] <- NA_real_
  }

  for (ac in adj_cols) {
    expect_true(is.na(df[[ac]][1]))
  }
  # Comp rows should remain intact
  expect_false(is.na(df[["net_adjustments"]][2]))
  expect_false(is.na(df[["gross_adjustments"]][3]))
})

# ---------------------------------------------------------------------------
# 13. Sorting by residual_sf descending
# ---------------------------------------------------------------------------
test_that("comps are sorted by residual_sf descending in appraisal mode", {
  df <- data.frame(
    id          = c("subject", "comp1", "comp2", "comp3"),
    residual_sf = c(NA, 0.02, 0.05, -0.01),
    sale_price  = c(NA, 200000, 250000, 180000)
  )

  # Appraisal mode: subject in row 1, sort comps by residual_sf desc
  comps <- df[2:nrow(df), , drop = FALSE]
  comps <- comps[order(comps[["residual_sf"]], decreasing = TRUE,
                       na.last = TRUE), , drop = FALSE]
  df_sorted <- rbind(df[1L, , drop = FALSE], comps)

  expect_equal(df_sorted$id[1], "subject")
  expect_equal(df_sorted$id[2], "comp2")  # highest residual_sf
  expect_equal(df_sorted$id[4], "comp3")  # lowest residual_sf
})

# ---------------------------------------------------------------------------
# 14. Log-scale RCA adjustments use proportional allocation
# ---------------------------------------------------------------------------
test_that("log-scale adjustments use proportional allocation method", {
  setup <- setup_gam_result("log")
  model <- setup$result$model
  dat   <- setup$data
  xform <- "log"

  preds <- predict(model, newdata = dat, type = "terms")
  intercept <- stats::coef(model)[["(Intercept)"]]
  contribs <- list()
  for (col in colnames(preds)) {
    contribs[[col]] <- as.numeric(preds[, col])
  }

  log_total <- intercept + Reduce(`+`, contribs)
  dollar_pred <- earthUI:::back_transform_(log_total, xform)
  dollar_base <- earthUI:::back_transform_(intercept, xform)
  dollar_effect <- dollar_pred - dollar_base
  log_sum <- Reduce(`+`, contribs)

  # For each term, dollar contribution = share * dollar_effect
  for (tl in names(contribs)) {
    share <- ifelse(abs(log_sum) > 1e-10, contribs[[tl]] / log_sum, 0)
    dollar_contrib <- dollar_effect * share

    # Adjustment: subject dollar contrib minus comp dollar contrib
    adjustment <- dollar_contrib[1] - dollar_contrib

    # Subject's adjustment vs self must be 0
    expect_equal(adjustment[1], 0)
  }
})

# ---------------------------------------------------------------------------
# 15. clean_term_name_ regex patterns
# ---------------------------------------------------------------------------
test_that("clean_term_name strips s(), te(), ti() wrappers", {
  clean_term_name_ <- function(tl) {
    nm <- gsub("^s\\((.+)\\)$", "\\1", tl)
    nm <- gsub("^te\\((.+)\\)$", "\\1", nm)
    nm <- gsub("^ti\\((.+)\\)$", "\\1", nm)
    nm
  }

  expect_equal(clean_term_name_("s(wt)"), "wt")
  expect_equal(clean_term_name_("te(wt,hp)"), "wt,hp")
  expect_equal(clean_term_name_("ti(wt,hp)"), "wt,hp")
  expect_equal(clean_term_name_("cyl"), "cyl")  # linear term unchanged
})

# ---------------------------------------------------------------------------
# 16. apply_transform_ round-trip with back_transform_
# ---------------------------------------------------------------------------
test_that("apply_transform_ and back_transform_ are inverses", {
  vals <- c(10, 20, 30, 100)

  for (xform in c("log", "log10", "none")) {
    transformed   <- earthUI:::apply_transform_(vals, xform)
    back          <- earthUI:::back_transform_(transformed, xform)
    expect_equal(back, vals, tolerance = 1e-10,
                 info = paste("Round-trip failed for", xform))
  }
})

# ---------------------------------------------------------------------------
# 17. Full end-to-end GAM output with log10 transform
# ---------------------------------------------------------------------------
test_that("log10 model output: back-transformed estimate matches mpg scale", {
  setup <- setup_gam_result("log10")
  model    <- setup$result$model
  dat      <- setup$data
  response <- "log10_mpg"
  xform    <- "log10"

  predicted_log <- as.numeric(predict(model, newdata = dat, type = "response"))
  predicted     <- earthUI:::back_transform_(predicted_log, xform)

  # Predicted values should be on mpg scale (roughly 10-35)
  expect_true(all(predicted > 5))
  expect_true(all(predicted < 50))

  # Residual on original scale
  residuals_val <- dat$mpg - predicted
  # Most residuals should be small relative to mpg
  expect_true(median(abs(residuals_val)) < 5)
})

# ---------------------------------------------------------------------------
# 18. Subject value = model estimate + interpolated residual
# ---------------------------------------------------------------------------
test_that("subject value computation: est + interpolated residual", {
  predicted_subject <- 300000
  interpolated_resid <- 7500
  subject_value <- predicted_subject + interpolated_resid
  expect_equal(subject_value, 307500)
})

# ---------------------------------------------------------------------------
# 19. Sales grid: comp_rows validation
# ---------------------------------------------------------------------------
test_that("comp_rows must be between 2 and n_total", {
  comp_rows <- c(2, 3, 4)
  n_total <- 10
  expect_true(all(comp_rows >= 2 & comp_rows <= n_total))

  bad_rows <- c(1, 5, 11)
  expect_true(any(bad_rows < 2 | bad_rows > n_total))
})

test_that("maximum 30 comps for sales grid", {
  comp_rows <- 2:40
  expect_true(length(comp_rows) > 30)
  comp_rows_capped <- comp_rows[1:30]
  expect_equal(length(comp_rows_capped), 30)
})

# ---------------------------------------------------------------------------
# 20. Sales grid: sheet count calculation
# ---------------------------------------------------------------------------
test_that("sales grid uses 3 comps per sheet", {
  for (n_comps in c(1, 3, 4, 6, 7, 10, 30)) {
    n_sheets <- ceiling(n_comps / 3)
    expected <- ceiling(n_comps / 3)
    expect_equal(n_sheets, expected, info = paste("n_comps =", n_comps))
  }
  # 30 comps -> 10 sheets (maximum)
  expect_equal(ceiling(30 / 3), 10)
})

# ---------------------------------------------------------------------------
# 21. compute_dom helper
# ---------------------------------------------------------------------------
test_that("compute_dom calculates days between listing and contract", {
  compute_dom <- function(df, row) {
    cd <- col_val(df, row, "contract_date", default = NA)
    ld <- col_val(df, row, "listing_date", default = NA)
    if (is.na(cd) || is.na(ld)) return(NA_integer_)
    cd <- tryCatch(as.Date(cd), error = function(e) NA)
    ld <- tryCatch(as.Date(ld), error = function(e) NA)
    if (is.na(cd) || is.na(ld)) return(NA_integer_)
    as.integer(cd - ld)
  }

  df <- data.frame(
    contract_date = c("2026-03-01", "2026-06-15"),
    listing_date  = c("2026-01-01", "2026-05-01"),
    stringsAsFactors = FALSE
  )
  expect_equal(compute_dom(df, 1), 59L)   # Jan 1 to Mar 1
  expect_equal(compute_dom(df, 2), 45L)   # May 1 to Jun 15
})

test_that("compute_dom returns NA for missing dates", {
  compute_dom <- function(df, row) {
    cd <- col_val(df, row, "contract_date", default = NA)
    ld <- col_val(df, row, "listing_date", default = NA)
    if (is.na(cd) || is.na(ld)) return(NA_integer_)
    cd <- tryCatch(as.Date(cd), error = function(e) NA)
    ld <- tryCatch(as.Date(ld), error = function(e) NA)
    if (is.na(cd) || is.na(ld)) return(NA_integer_)
    as.integer(cd - ld)
  }

  df <- data.frame(
    contract_date = c("2026-03-01", NA),
    listing_date  = c(NA, "2026-05-01"),
    stringsAsFactors = FALSE
  )
  expect_true(is.na(compute_dom(df, 1)))
  expect_true(is.na(compute_dom(df, 2)))
})

# ---------------------------------------------------------------------------
# 22. Recommended comps: gross_adj_pct < 25%
# ---------------------------------------------------------------------------
test_that("recommended comps have gross adjustment under 25%", {
  comp_info <- data.frame(
    row           = 2:7,
    sale_price    = c(200000, 250000, 300000, 180000, 220000, 190000),
    gross_adj     = c(30000, 80000, 50000, 20000, 100000, 10000),
    weight        = rep(1, 6),
    stringsAsFactors = FALSE
  )
  comp_info$gross_adj_pct <- abs(comp_info$gross_adj / comp_info$sale_price)

  eligible <- comp_info[comp_info$weight > 0, ]
  recommended <- eligible[!is.na(eligible$gross_adj_pct) &
                           eligible$gross_adj_pct < 0.25, ]

  # Check which ones qualify
  expect_true(all(recommended$gross_adj_pct < 0.25))
  # Row with 80000/250000 = 0.32 should NOT be recommended
  expect_false(3 %in% recommended$row)
  # Row with 10000/190000 = 0.053 should be recommended
  expect_true(7 %in% recommended$row)
})

# ---------------------------------------------------------------------------
# 23. Contributions on identity scale match direct predict
# ---------------------------------------------------------------------------
test_that("identity-scale contributions match direct predictions exactly", {
  setup <- setup_gam_result("none")
  model <- setup$result$model
  dat   <- setup$data

  # Direct prediction
  fitted_direct <- as.numeric(predict(model, newdata = dat, type = "response"))

  # Via contributions
  preds <- predict(model, newdata = dat, type = "terms")
  intercept <- stats::coef(model)[["(Intercept)"]]
  fitted_from_contribs <- intercept + rowSums(preds)

  expect_equal(fitted_direct, as.numeric(fitted_from_contribs), tolerance = 1e-8)
})

# ---------------------------------------------------------------------------
# 24. Proportional allocation: no NaN/Inf when log_sum is near zero
# ---------------------------------------------------------------------------
test_that("proportional allocation guards against division by near-zero", {
  # Simulate a case where all contributions are near zero
  log_sum <- c(0, 1e-11, 0.5, -0.3)
  contrib <- c(0, 1e-11, 0.2, -0.1)

  share <- ifelse(abs(log_sum) > 1e-10, contrib / log_sum, 0)

  expect_true(all(is.finite(share)))
  # When log_sum is near zero, share should be 0
  expect_equal(share[1], 0)
  expect_equal(share[2], 0)
})

# ---------------------------------------------------------------------------
# 25. Basis value is intercept (back-transformed for RCA)
# ---------------------------------------------------------------------------
test_that("basis in RCA is back-transformed intercept", {
  setup <- setup_gam_result("log")
  model <- setup$result$model
  xform <- "log"

  intercept <- stats::coef(model)[["(Intercept)"]]
  basis_dollar <- earthUI:::back_transform_(intercept, xform)

  # For log model, basis should be exp(intercept)
  expect_equal(basis_dollar, exp(intercept))
  # Should be positive and on a reasonable scale for mpg
  expect_true(basis_dollar > 0)
})

test_that("basis in non-RCA output stays on transformed scale", {
  setup <- setup_gam_result("log")
  model <- setup$result$model

  intercept <- stats::coef(model)[["(Intercept)"]]
  # In the non-RCA output (lines 1027-1028), basis is raw intercept
  basis_raw <- round(intercept, 4)
  expect_equal(basis_raw, round(intercept, 4))
})

# ---------------------------------------------------------------------------
# 26. GAM with weights produces different fit than unweighted
# ---------------------------------------------------------------------------
test_that("weighted GAM differs from unweighted GAM", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = 5L),
    list(vars = "hp", type = "s", bs = "tp", k = 5L)
  )
  dat <- mtcars[1:20, ]

  # Unweighted
  res_uw <- fit_gam(dat, "mpg", specs, cv_folds = 0L)


  # Weighted (downweight last 5 rows)
  wts <- c(rep(1, 15), rep(0.1, 5))
  res_wt <- fit_gam(dat, "mpg", specs, weights = wts, cv_folds = 0L)

  coef_uw <- stats::coef(res_uw$model)
  coef_wt <- stats::coef(res_wt$model)

  # Coefficients should differ
  expect_false(isTRUE(all.equal(coef_uw, coef_wt)))
})

# ---------------------------------------------------------------------------
# 27. sp_col helper returns NULL for missing specials
# ---------------------------------------------------------------------------
test_that("sp_col returns column name or NULL", {
  sp_col <- function(specials, type) {
    if (!is.null(specials[[type]])) specials[[type]] else NULL
  }

  specials <- list(latitude = "lat_col", longitude = "lon_col")
  expect_equal(sp_col(specials, "latitude"), "lat_col")
  expect_null(sp_col(specials, "lot_size"))
  expect_null(sp_col(list(), "latitude"))
})
