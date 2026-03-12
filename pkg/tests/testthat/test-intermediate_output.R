# test-intermediate_output.R — Tests for intermediate output calculations
#
# These tests verify the CQA scoring, residual computation, and contribution
# column logic used in Step 6 (Download Estimated Sale Prices & Residuals).
# The actual server.R code is reactive, so we test the underlying logic
# independently.

test_that("CQA scoring ranks residuals correctly", {
  # CQA = proportion of comps with smaller residual * 10
  residuals_val <- c(NA, -200, -100, 0, 100, 200)  # row 1 = subject (NA)
  comp_resid <- residuals_val[-1L]
  n_comps <- sum(!is.na(comp_resid))

  cqa_all <- vapply(residuals_val, function(r) {
    if (is.na(r)) return(NA_real_)
    sum(comp_resid < r, na.rm = TRUE) / n_comps * 10
  }, numeric(1))

  expect_true(is.na(cqa_all[1]))       # subject is NA
  expect_equal(cqa_all[2], 0)           # lowest residual
  expect_equal(cqa_all[6], 8)           # highest (4/5 * 10)
  # CQA should be monotonically increasing with residual
  expect_true(all(diff(cqa_all[-1]) >= 0))
})

test_that("CQA per-SF scoring works with living area", {
  residuals_val <- c(NA, -200, -100, 0, 100, 200)
  living_area   <- c(2000, 1500, 1800, 2000, 1600, 2200)

  resid_sf <- residuals_val / living_area
  comp_resid_sf <- resid_sf[-1L]
  n_comps_sf <- sum(!is.na(comp_resid_sf))

  cqa_sf <- vapply(resid_sf, function(r) {
    if (is.na(r)) return(NA_real_)
    sum(comp_resid_sf < r, na.rm = TRUE) / n_comps_sf * 10
  }, numeric(1))

  expect_true(is.na(cqa_sf[1]))
  # Values should be between 0 and 10
  expect_true(all(cqa_sf[-1] >= 0 & cqa_sf[-1] <= 10))
})

test_that("contribution columns sum to prediction", {
  # Simulate: basis + contributions should equal prediction
  result <- fit_earth(mtcars, target = "mpg",
                      predictors = c("wt", "hp", "cyl"))
  model <- result$model
  eq <- format_model_equation(result)
  groups <- eq$groups

  # Extract intercept
  basis <- 0
  contrib_groups <- list()
  for (grp in groups) {
    if (grp$degree == 0L) {
      basis <- grp$terms[[1]]$coefficient
    } else {
      contrib_groups <- c(contrib_groups, list(grp))
    }
  }

  # Compute contributions
  pred_df <- result$data
  predicted <- as.numeric(predict(model, newdata = pred_df))
  total_contrib <- rep(basis, nrow(pred_df))

  for (grp in contrib_groups) {
    contrib <- earthUI:::eval_g_function_(model, grp, pred_df)
    total_contrib <- total_contrib + contrib
  }

  # basis + sum(contributions) should equal prediction
  expect_equal(total_contrib, predicted, tolerance = 0.01)
})

test_that("residual = actual - predicted", {
  result <- fit_earth(mtcars, target = "mpg",
                      predictors = c("wt", "hp"))
  predicted <- as.numeric(predict(result$model, newdata = result$data))
  actual <- result$data$mpg
  residuals_val <- actual - predicted

  expect_equal(length(residuals_val), nrow(result$data))
  # Sum of residuals should be near zero for earth models
  expect_true(abs(mean(residuals_val)) < 1)
})

test_that("calc_residual verification column matches residual", {
  result <- fit_earth(mtcars, target = "mpg",
                      predictors = c("wt", "hp"))
  model <- result$model
  eq <- format_model_equation(result)
  groups <- eq$groups

  pred_df <- result$data
  predicted <- as.numeric(predict(model, newdata = pred_df))
  actual <- pred_df$mpg

  # Compute contributions
  basis <- 0
  total_contrib <- numeric(nrow(pred_df))
  for (grp in groups) {
    if (grp$degree == 0L) {
      basis <- grp$terms[[1]]$coefficient
    } else {
      contrib <- earthUI:::eval_g_function_(model, grp, pred_df)
      total_contrib <- total_contrib + contrib
    }
  }

  calc_residual <- actual - (basis + total_contrib)
  direct_residual <- actual - predicted

  expect_equal(calc_residual, direct_residual, tolerance = 0.01)
})

test_that("ranking columns are in correct order", {
  # Verify the expected column ordering logic
  ranking_cols <- c("residual_sf", "cqa_sf", "residual", "cqa")
  other_cols <- c("street_address", "sale_price", "living_sqft")

  # Simulate reordering
  all_cols <- c(other_cols, ranking_cols)
  df <- as.data.frame(matrix(0, nrow = 3, ncol = length(all_cols)))
  names(df) <- all_cols

  available <- intersect(ranking_cols, names(df))
  remaining <- setdiff(names(df), available)
  reordered <- c(available, remaining)

  expect_equal(reordered[1:4], ranking_cols)
  expect_equal(reordered[5:7], other_cols)
})
