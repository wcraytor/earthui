test_that("fit_earth fits a basic model", {
  result <- fit_earth(mtcars, target = "mpg",
                      predictors = c("cyl", "disp", "hp", "wt"))
  expect_s3_class(result, "earthui_result")
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
  expect_s3_class(result, "earthui_result")
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
  expect_s3_class(result, "earthui_result")
})
