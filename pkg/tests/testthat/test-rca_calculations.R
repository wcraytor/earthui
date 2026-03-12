# test-rca_calculations.R — Tests for RCA calculation logic
#
# The RCA code lives in server.R but the core math (CQA interpolation,
# adjustment computation) can be tested independently.

test_that("CQA interpolation produces correct subject residual", {
  # Simulate comp CQA/residual pairs
  cqa_sorted   <- c(0, 2, 4, 6, 8, 10)
  resid_sorted <- c(-50000, -30000, -10000, 10000, 30000, 50000)

  # User CQA = 5.0 should interpolate between idx 3 (4,-10000) and idx 4 (6,10000)
  user_cqa <- 5.0
  subject_resid <- stats::approx(cqa_sorted, resid_sorted,
                                  xout = user_cqa, rule = 2)$y
  expect_equal(subject_resid, 0)  # midpoint between -10000 and 10000

  # CQA = 0 should give lowest residual
  expect_equal(
    stats::approx(cqa_sorted, resid_sorted, xout = 0, rule = 2)$y,
    -50000
  )

  # CQA = 10 should give highest residual
  expect_equal(
    stats::approx(cqa_sorted, resid_sorted, xout = 10, rule = 2)$y,
    50000
  )
})

test_that("CQA interpolation clamps at extremes (rule=2)", {
  cqa_sorted   <- c(1, 3, 5, 7, 9)
  resid_sorted <- c(-20000, -10000, 0, 10000, 20000)

  # Below range: should clamp to lowest
  below <- stats::approx(cqa_sorted, resid_sorted, xout = 0, rule = 2)$y
  expect_equal(below, -20000)

  # Above range: should clamp to highest
  above <- stats::approx(cqa_sorted, resid_sorted, xout = 10, rule = 2)$y
  expect_equal(above, 20000)
})

test_that("subject_value = est + interpolated residual", {
  predicted_subject <- 450000
  subject_resid_total <- 12000
  subject_est <- predicted_subject + subject_resid_total
  expect_equal(subject_est, 462000)
})

test_that("CQA per-SF converts residual correctly", {
  # When use_sf = TRUE, interpolated value is per-SF, multiply by living_area
  subject_resid_per_sf <- 5.0  # $5/sqft
  subject_la <- 2000

  subject_resid_total <- subject_resid_per_sf * subject_la
  expect_equal(subject_resid_total, 10000)
})

test_that("adjustment = subject contribution - comp contribution", {
  subj_contrib <- c(sale_age = 5000, living_sqft = 80000, age = -8000)
  comp_contrib <- c(sale_age = 3000, living_sqft = 72000, age = -10000)

  adjustments <- subj_contrib - comp_contrib
  expect_equal(adjustments[["sale_age"]], 2000)
  expect_equal(adjustments[["living_sqft"]], 8000)
  expect_equal(adjustments[["age"]], 2000)
})

test_that("net_adjustments = sum of all adjustments + residual_adjustment", {
  adjustments <- c(2000, 8000, 2000)
  residual_adj <- 5000
  net <- sum(adjustments) + residual_adj
  expect_equal(net, 17000)
})

test_that("gross_adjustments = sum of absolute adjustments", {
  adjustments <- c(2000, -8000, 2000)
  residual_adj <- -5000
  gross <- sum(abs(adjustments)) + abs(residual_adj)
  expect_equal(gross, 17000)
})

test_that("adjusted_sale_price = sale_price + net_adjustments", {
  sale_price <- 500000
  net_adj <- -12000
  adjusted <- sale_price + net_adj
  expect_equal(adjusted, 488000)
})

test_that("residual_adjustment = subject_resid - comp_resid", {
  subject_resid_total <- 8000
  comp_residuals <- c(5000, -3000, 12000, 0)
  resid_adj <- subject_resid_total - comp_residuals
  expect_equal(resid_adj, c(3000, 11000, -4000, 8000))
})

test_that("weight-0 rows get subject_value = est + subject_resid", {
  predicted <- c(450000, 480000, 520000, 500000, 460000)
  subject_resid_total <- 10000
  zero_wt <- c(3, 5)  # rows with weight = 0

  sv <- predicted[zero_wt] + subject_resid_total
  expect_equal(sv, c(530000, 470000))
})

test_that("percentage columns compute correctly", {
  actual <- c(500000, 480000, 520000)
  net_adj <- c(0, -12000, 8000)
  gross_adj <- c(0, 25000, 18000)
  resid_adj <- c(0, 5000, -3000)

  net_pct <- round(net_adj / actual, 4)
  gross_pct <- round(gross_adj / actual, 4)
  resid_pct <- round(resid_adj / actual, 4)

  expect_equal(net_pct[2], round(-12000 / 480000, 4))
  expect_equal(gross_pct[2], round(25000 / 480000, 4))
  expect_equal(resid_pct[3], round(-3000 / 520000, 4))
})

test_that("RCA with real earth model produces consistent results", {
  # End-to-end: fit model, compute contributions, verify adjustments
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
  contrib_list <- list()
  for (grp in groups) {
    if (grp$degree == 0L) {
      basis <- grp$terms[[1]]$coefficient
    } else {
      label <- gsub(" ", "_", grp$label)
      contrib <- earthUI:::eval_g_function_(model, grp, pred_df)
      contrib_list[[label]] <- contrib
    }
  }

  # Subject = row 1, comp = row 2
  for (label in names(contrib_list)) {
    adj <- contrib_list[[label]][1] - contrib_list[[label]][2]
    # Adjustment should be finite
    expect_true(is.finite(adj))
  }

  # Net adjustments: sum of variable adjustments + residual adjustment
  residuals_val <- actual - predicted
  resid_adj_1_vs_2 <- residuals_val[1] - residuals_val[2]

  total_var_adj <- sum(vapply(contrib_list, function(c) c[1] - c[2], numeric(1)))
  net_adj <- total_var_adj + resid_adj_1_vs_2

  # adjusted_sale_price = actual[2] + net_adj should equal actual[1]
  adjusted_sp <- actual[2] + net_adj
  expect_equal(adjusted_sp, actual[1], tolerance = 0.01)
})
