# test-detect_types_extra.R — Additional type detection tests

test_that("detect_column_types_mgcv handles all-NA columns", {
  df <- data.frame(x = c(NA, NA, NA))
  types <- detect_column_types_mgcv(df)
  expect_true(is.character(types))
  expect_equal(length(types), 1L)
})

test_that("detect_column_types_mgcv handles mixed-type columns", {
  df <- data.frame(x = 1:5, y = letters[1:5], stringsAsFactors = FALSE)
  types <- detect_column_types_mgcv(df)
  expect_equal(unname(types["x"]), "integer")
  expect_equal(unname(types["y"]), "character")
})

test_that("detect_column_types_mgcv preserves column names", {
  df <- data.frame(price = 1.0, sqft = 100L, city = "A",
                   stringsAsFactors = FALSE)
  types <- detect_column_types_mgcv(df)
  expect_equal(names(types), c("price", "sqft", "city"))
})

test_that("detect_column_types_mgcv handles single-column data frame", {
  df <- data.frame(x = 1:10)
  types <- detect_column_types_mgcv(df)
  expect_equal(length(types), 1L)
  expect_equal(unname(types), "integer")
})

test_that("detect_column_types_mgcv handles wide data frame", {
  df <- as.data.frame(matrix(1:100, ncol = 20))
  types <- detect_column_types_mgcv(df)
  expect_equal(length(types), 20L)
  expect_true(all(types == "integer"))
})

test_that("detect_column_types_mgcv handles Date columns", {
  df <- data.frame(
    d = as.Date("2024-01-01") + 0:4,
    p = as.POSIXct("2024-01-01") + (0:4) * 3600
  )
  types <- detect_column_types_mgcv(df)
  expect_equal(unname(types["d"]), "Date")
  expect_equal(unname(types["p"]), "POSIXct")
})

test_that("detect_column_types_mgcv handles logical columns", {
  df <- data.frame(flag = c(TRUE, FALSE, TRUE))
  types <- detect_column_types_mgcv(df)
  expect_equal(unname(types), "logical")
})
