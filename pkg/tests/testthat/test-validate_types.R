test_that("validate_types returns ok for matching types", {
  df <- data.frame(price = c(100, 200, 300), city = c("A", "B", "C"),
                   stringsAsFactors = FALSE)
  types <- list(price = "numeric", city = "character")
  result <- validate_types(df, types, predictors = c("price", "city"))
  expect_true(result$ok)
  expect_length(result$errors, 0L)
  expect_length(result$warnings, 0L)
})

test_that("validate_types errors on character declared as numeric with row details", {
  df <- data.frame(x = c("a", "b", "c"), stringsAsFactors = FALSE)
  types <- list(x = "numeric")
  result <- validate_types(df, types, predictors = "x")
  expect_false(result$ok)
  expect_length(result$errors, 1L)
  expect_match(result$errors, "numeric")
  expect_match(result$errors, "rows")
})

test_that("validate_types warns on double declared as integer", {
  df <- data.frame(x = c(1.5, 2.5, 3.5))
  types <- list(x = "integer")
  result <- validate_types(df, types, predictors = "x")
  expect_true(result$ok)
  expect_length(result$warnings, 1L)
  expect_match(result$warnings, "integer")
})

test_that("validate_types reports Date columns for conversion", {
  df <- data.frame(d = as.Date(c("2024-01-01", "2024-06-15")))
  types <- list(d = "Date")
  result <- validate_types(df, types, predictors = "d")
  expect_true(result$ok)
  expect_equal(result$date_columns, "d")
})

test_that("validate_types reports POSIXct columns for conversion", {
  df <- data.frame(ts = as.POSIXct(c("2024-01-01", "2024-06-15")))
  types <- list(ts = "POSIXct")
  result <- validate_types(df, types, predictors = "ts")
  expect_true(result$ok)
  expect_equal(result$date_columns, "ts")
})

test_that("validate_types errors on character dates that don't parse", {
  df <- data.frame(d = c("not-a-date", "also-not"), stringsAsFactors = FALSE)
  types <- list(d = "Date")
  result <- validate_types(df, types, predictors = "d")
  expect_false(result$ok)
  expect_match(result$errors[1], "don't parse")
})

test_that("validate_types skips unknown type", {
  df <- data.frame(x = c(1, 2, 3))
  types <- list(x = "unknown")
  result <- validate_types(df, types, predictors = "x")
  expect_true(result$ok)
  expect_length(result$errors, 0L)
  expect_length(result$warnings, 0L)
})

test_that("validate_types handles missing column in type_map", {
  df <- data.frame(x = c(1, 2, 3))
  types <- list()  # no entry for x
  result <- validate_types(df, types, predictors = "x")
  expect_true(result$ok)
})

test_that("validate_types ignores columns not in predictors", {
  df <- data.frame(x = c("a", "b"), y = c(1, 2), stringsAsFactors = FALSE)
  types <- list(x = "numeric", y = "numeric")  # x is wrong but not a predictor
  result <- validate_types(df, types, predictors = "y")
  expect_true(result$ok)
  expect_length(result$errors, 0L)
})

test_that("validate_types errors on logical mismatch", {
  df <- data.frame(x = c(5, 10, 15))
  types <- list(x = "logical")
  result <- validate_types(df, types, predictors = "x")
  expect_false(result$ok)
  expect_match(result$errors[1], "logical")
})

test_that("validate_types accepts numeric 0/1 declared as logical", {
  df <- data.frame(x = c(0, 1, 1, 0))
  types <- list(x = "logical")
  result <- validate_types(df, types, predictors = "x")
  expect_true(result$ok)
})

test_that("validate_types warns on numeric declared as factor", {
  df <- data.frame(x = c(1, 2, 3))
  types <- list(x = "factor")
  result <- validate_types(df, types, predictors = "x")
  expect_true(result$ok)
  expect_length(result$warnings, 1L)
  expect_match(result$warnings, "factor")
})

test_that("validate_types errors on missing column", {
  df <- data.frame(x = c(1, 2, 3))
  types <- list(missing_col = "numeric")
  result <- validate_types(df, types, predictors = "missing_col")
  expect_false(result$ok)
  expect_match(result$errors[1], "not found")
})

test_that("validate_types warns on Date column with some unparseable rows", {
  df <- data.frame(d = c("2024-01-01", "2024-06-15", "TBD", "pending", "2024-12-31"),
                   stringsAsFactors = FALSE)
  types <- list(d = "Date")
  result <- validate_types(df, types, predictors = "d")
  # Should still be ok (warning, not error) since most values parse
  expect_true(result$ok)
  expect_length(result$warnings, 1L)
  expect_match(result$warnings, "could not be parsed")
  expect_match(result$warnings, "rows 3, 4")
  expect_equal(result$date_columns, "d")
})

test_that("validate_types reports row numbers for logical mismatch", {
  df <- data.frame(x = c(0, 1, 5, 0, 10))
  types <- list(x = "logical")
  result <- validate_types(df, types, predictors = "x")
  expect_false(result$ok)
  expect_match(result$errors[1], "rows 3, 5")
})

test_that("validate_types reports row numbers for non-numeric character values", {
  df <- data.frame(x = c("100", "200", "N/A", "300", "TBD"),
                   stringsAsFactors = FALSE)
  types <- list(x = "numeric")
  result <- validate_types(df, types, predictors = "x")
  expect_false(result$ok)
  expect_match(result$errors[1], "rows 3, 5")
  expect_match(result$errors[1], "N/A")
})
