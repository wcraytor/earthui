test_that("fit_earth fits a basic model", {
  result <- fit_earth(mtcars, target = "mpg",
                      predictors = c("cyl", "disp", "hp", "wt"))
  expect_s3_class(result, "earthUI_result")
  expect_true(!is.null(result$model))
  expect_equal(result$target, "mpg")
  expect_equal(result$predictors, c("cyl", "disp", "hp", "wt"))
  expect_equal(result$degree, 1L)
  expect_false(result$cv_enabled)
})

test_that("fit_earth auto-enables CV for degree >= 2", {
  result <- fit_earth(mtcars, target = "mpg",
                      predictors = c("wt", "hp"),
                      degree = 2L)
  expect_true(result$cv_enabled)
  expect_equal(result$degree, 2L)
})

test_that("fit_earth handles categorical variables", {
  df <- mtcars
  df$am_cat <- as.character(df$am)
  result <- fit_earth(df, target = "mpg",
                      predictors = c("wt", "hp", "am_cat"),
                      categoricals = "am_cat")
  expect_s3_class(result, "earthUI_result")
  expect_equal(result$categoricals, "am_cat")
})

test_that("fit_earth validates inputs", {
  expect_error(fit_earth(mtcars, "nonexistent", "wt"), "not found")
  expect_error(fit_earth(mtcars, "mpg", character(0)), "non-empty")
  expect_error(fit_earth(mtcars, "mpg", "nonexistent"), "not found")
  expect_error(fit_earth(mtcars, "mpg", c("mpg", "wt")), "must not be in")
  expect_error(fit_earth("not_df", "mpg", "wt"), "data frame")
})

test_that("fit_earth rejects insufficient data", {
  expect_error(fit_earth(mtcars[1:5, ], "mpg", names(mtcars)[-1]),
               "Insufficient data")
})

test_that("fit_earth handles missing values", {
  df <- mtcars
  df$wt[1:3] <- NA
  expect_message(
    result <- fit_earth(df, "mpg", c("wt", "hp")),
    "Removed 3 rows"
  )
  expect_s3_class(result, "earthUI_result")
})

test_that("fit_earth subsets weights when NA rows removed", {
  df <- mtcars
  df$hp[1:5] <- NA
  w <- rep(1, nrow(df))
  # Should not error — weights must be subsetted to match after NA removal
  expect_message(
    result <- fit_earth(df, "mpg", c("wt", "hp"), weights = w),
    "Removed 5 rows"
  )
  expect_s3_class(result, "earthUI_result")
})

test_that("fit_earth with wp (response weights) for multi-target", {
  wp_vec <- c(2, 1)
  result <- fit_earth(mtcars, c("mpg", "hp"), c("cyl", "wt"), wp = wp_vec)
  expect_s3_class(result, "earthUI_result")
  expect_equal(ncol(result$model$coefficients), 2L)
})

test_that("fit_earth with weights + wp together", {
  w <- runif(nrow(mtcars), 0.5, 1.5)
  wp_vec <- c(1, 3)
  result <- fit_earth(mtcars, c("mpg", "hp"), c("cyl", "wt"),
                      weights = w, wp = wp_vec)
  expect_s3_class(result, "earthUI_result")
})

test_that("fit_earth with weights + NA removal + multi-target", {
  df <- mtcars
  df$cyl[1:3] <- NA
  w <- rep(1, nrow(df))
  wp_vec <- c(1, 2)
  expect_message(
    result <- fit_earth(df, c("mpg", "hp"), c("cyl", "wt"),
                        weights = w, wp = wp_vec),
    "Removed 3 rows"
  )
  expect_s3_class(result, "earthUI_result")
})
