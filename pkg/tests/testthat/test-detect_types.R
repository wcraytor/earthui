test_that("detect_types identifies basic R types", {
  df <- data.frame(
    dbl  = c(1.5, 2.5, 3.5),
    int  = c(1L, 2L, 3L),
    chr  = c("a", "b", "c"),
    lgl  = c(TRUE, FALSE, TRUE),
    fct  = factor(c("x", "y", "z")),
    stringsAsFactors = FALSE
  )
  types <- detect_types(df)
  expect_equal(types[["dbl"]], "numeric")
  expect_equal(types[["int"]], "integer")
  expect_equal(types[["chr"]], "character")
  expect_equal(types[["lgl"]], "logical")
  expect_equal(types[["fct"]], "factor")
})

test_that("detect_types detects Date and POSIXct columns", {
  df <- data.frame(
    d = as.Date(c("2024-01-01", "2024-06-15", "2024-12-31")),
    ts = as.POSIXct(c("2024-01-01 10:00:00", "2024-06-15 12:00:00",
                       "2024-12-31 23:59:59"))
  )
  types <- detect_types(df)
  expect_equal(types[["d"]], "Date")
  expect_equal(types[["ts"]], "POSIXct")
})

test_that("detect_types detects date-like character columns", {
  df <- data.frame(
    iso  = c("2024-01-15", "2024-06-20", "2024-12-31"),
    us   = c("01/15/2024", "06/20/2024", "12/31/2024"),
    text = c("hello", "world", "foo"),
    stringsAsFactors = FALSE
  )
  types <- detect_types(df)
  expect_equal(types[["iso"]], "Date")
  expect_equal(types[["us"]], "Date")
  expect_equal(types[["text"]], "character")
})

test_that("detect_types flags 0/1 numeric as logical", {
  df <- data.frame(
    bool_int = c(0L, 1L, 0L, 1L),
    bool_dbl = c(0, 1, 1, 0),
    real_int = c(1L, 2L, 3L, 4L),
    only_ones = c(1, 1, 1, 1)
  )
  types <- detect_types(df)
  expect_equal(types[["bool_int"]], "logical")
  expect_equal(types[["bool_dbl"]], "logical")
  expect_equal(types[["real_int"]], "integer")
  # only_ones has only one unique value (1), not both 0 and 1
  expect_true(types[["only_ones"]] %in% c("numeric", "integer"))
})

test_that("detect_types handles all-NA columns", {
  df <- data.frame(
    na_chr = c(NA_character_, NA_character_),
    na_num = c(NA_real_, NA_real_)
  )
  types <- detect_types(df)
  expect_equal(types[["na_chr"]], "character")
  expect_equal(types[["na_num"]], "numeric")
})

test_that("detect_types returns named vector matching column names", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  types <- detect_types(df)
  expect_equal(names(types), c("a", "b"))
  expect_true(is.character(types))
})

test_that("detect_types rejects non-data.frame input", {
  expect_error(detect_types(1:10), "data frame")
  expect_error(detect_types("hello"), "data frame")
})

test_that("coerce_types_ converts Date to numeric", {
  df <- data.frame(
    d = as.Date(c("2024-01-01", "2024-06-15")),
    x = c(1, 2)
  )
  type_map <- list(d = "Date", x = "numeric")
  result <- earthUI:::coerce_types_(df, type_map, c("d", "x"))
  expect_true(is.numeric(result$d))
  expect_equal(result$d, as.numeric(as.Date(c("2024-01-01", "2024-06-15"))))
  expect_equal(result$x, c(1, 2))
})

test_that("coerce_types_ converts POSIXct to numeric", {
  df <- data.frame(
    ts = as.POSIXct(c("2024-01-01 10:00:00", "2024-06-15 12:00:00"), tz = "UTC"),
    x = c(1, 2)
  )
  type_map <- list(ts = "POSIXct", x = "numeric")
  result <- earthUI:::coerce_types_(df, type_map, c("ts", "x"))
  expect_true(is.numeric(result$ts))
})

test_that("coerce_types_ converts character dates to numeric", {
  df <- data.frame(
    d = c("2024-01-01", "2024-06-15"),
    stringsAsFactors = FALSE
  )
  type_map <- list(d = "Date")
  result <- earthUI:::coerce_types_(df, type_map, "d")
  expect_true(is.numeric(result$d))
  expect_equal(result$d, as.numeric(as.Date(c("2024-01-01", "2024-06-15"))))
})

test_that("coerce_types_ converts numeric 0/1 to logical", {
  df <- data.frame(flag = c(0, 1, 1, 0))
  type_map <- list(flag = "logical")
  result <- earthUI:::coerce_types_(df, type_map, "flag")
  expect_true(is.logical(result$flag))
  expect_equal(result$flag, c(FALSE, TRUE, TRUE, FALSE))
})

test_that("coerce_types_ converts to factor", {
  df <- data.frame(city = c("A", "B", "C"), stringsAsFactors = FALSE)
  type_map <- list(city = "factor")
  result <- earthUI:::coerce_types_(df, type_map, "city")
  expect_true(is.factor(result$city))
})

test_that("coerce_types_ skips unknown type", {
  df <- data.frame(x = c(1, 2, 3))
  type_map <- list(x = "unknown")
  result <- earthUI:::coerce_types_(df, type_map, "x")
  expect_equal(result$x, c(1, 2, 3))
})
