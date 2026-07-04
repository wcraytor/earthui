test_that("detect_column_types_mgcv identifies basic types", {
  df <- data.frame(
    a = 1.5,
    b = 1L,
    c = "text",
    d = TRUE,
    stringsAsFactors = FALSE
  )
  types <- detect_column_types_mgcv(df)
  expect_equal(unname(types), c("numeric", "integer", "character", "logical"))
})

test_that("detect_column_types_mgcv handles Date and factor", {
  df <- data.frame(
    dt = as.Date("2024-01-01"),
    fc = factor("a")
  )
  types <- detect_column_types_mgcv(df)
  expect_equal(unname(types), c("Date", "factor"))
})

test_that("detect_column_types_mgcv handles POSIXct", {
  df <- data.frame(ts = as.POSIXct("2024-01-01 12:00:00"))
  types <- detect_column_types_mgcv(df)
  expect_equal(unname(types), "POSIXct")
})
