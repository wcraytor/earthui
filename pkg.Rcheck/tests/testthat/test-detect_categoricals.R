test_that("detect_categoricals flags character columns as categorical", {
  df <- data.frame(a = c("x", "y"), b = 1:2, stringsAsFactors = FALSE)
  result <- detect_categoricals(df)
  expect_true(result[["a"]])
})

test_that("detect_categoricals flags factor columns as categorical", {
  df <- data.frame(a = factor(c("x", "y")), b = 1:2)
  result <- detect_categoricals(df)
  expect_true(result[["a"]])
})

test_that("detect_categoricals flags logical columns as categorical", {
  df <- data.frame(a = c(TRUE, FALSE), b = 1:2)
  result <- detect_categoricals(df)
  expect_true(result[["a"]])
})

test_that("detect_categoricals flags low-unique numeric as categorical", {
  df <- data.frame(
    rating = c(1, 2, 3, 1, 2, 3),
    price = c(100, 200, 300, 400, 500, 600)
  )
  result <- detect_categoricals(df, max_unique = 5)
  expect_true(result[["rating"]])
  expect_false(result[["price"]])
})

test_that("detect_categoricals uses default max_unique = 10", {
  df <- data.frame(x = 1:11)
  result <- detect_categoricals(df)
  expect_false(result[["x"]])

  df2 <- data.frame(x = 1:10)
  result2 <- detect_categoricals(df2)
  expect_true(result2[["x"]])
})

test_that("detect_categoricals handles NA values", {
  df <- data.frame(x = c(1, 2, NA, 1, NA))
  result <- detect_categoricals(df)
  expect_true(result[["x"]])  # 2 unique non-NA values
})

test_that("detect_categoricals returns named logical vector", {
  df <- data.frame(a = 1:100, b = c("x", "y"))
  result <- detect_categoricals(df)
  expect_type(result, "logical")
  expect_equal(names(result), c("a", "b"))
})

test_that("detect_categoricals rejects non-data.frame", {
  expect_error(detect_categoricals(list()), "must be a data frame")
})
