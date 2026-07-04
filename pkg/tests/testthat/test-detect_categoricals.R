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

test_that("detect_categoricals does NOT flag numeric columns", {
  # A low-cardinality numeric (e.g. bath_count) is a continuous predictor, not
  # a factor — the appraiser designates numeric factors explicitly.
  df <- data.frame(
    rating = c(1, 2, 3, 1, 2, 3),
    price = c(100, 200, 300, 400, 500, 600)
  )
  result <- detect_categoricals(df)
  expect_false(result[["rating"]])
  expect_false(result[["price"]])
})

test_that("detect_categoricals ignores numeric cardinality", {
  expect_false(detect_categoricals(data.frame(x = 1:10))[["x"]])
  expect_false(detect_categoricals(data.frame(x = 1:11))[["x"]])
})

test_that("detect_categoricals handles NA values", {
  # numeric with NAs -> not a factor
  expect_false(detect_categoricals(data.frame(x = c(1, 2, NA, 1, NA)))[["x"]])
  # character with NAs -> factor
  expect_true(detect_categoricals(
    data.frame(x = c("a", NA, "b"), stringsAsFactors = FALSE))[["x"]])
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

test_that("identifier-like text columns are not flagged", {
  n <- 40
  df <- data.frame(
    street_address = paste(seq_len(n), "Main St Apt", seq_len(n)),  # all unique
    mls_id   = sprintf("ML%06d", seq_len(n)),                        # all unique
    city     = sample(c("pacifica", "daly_city", "brisbane"), n, TRUE),
    style    = sample(c("ranch", "colonial"), n, TRUE),
    has_pool = sample(c(TRUE, FALSE), n, TRUE),
    stringsAsFactors = FALSE
  )
  out <- detect_categoricals(df)
  expect_false(out[["street_address"]])   # per-row label
  expect_false(out[["mls_id"]])           # per-row label
  expect_true(out[["city"]])              # genuine categorical
  expect_true(out[["style"]])
  expect_true(out[["has_pool"]])          # logicals always flagged

  # small data protection: <= 10 uniques stays flagged even if all distinct
  small <- data.frame(zone = paste0("z", 1:8), stringsAsFactors = FALSE)
  expect_true(detect_categoricals(small)[["zone"]])

  # internal twin must agree (parity across the three routines)
  expect_identical(earthUI:::detect_categoricals_(df), detect_categoricals(df))
})
