# Tests for import_data() edge cases

test_that("import_data converts camelCase to snake_case", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  df_orig <- data.frame(
    firstName = c("A", "B"),
    lastName = c("C", "D"),
    salePrice = c(100, 200),
    check.names = FALSE
  )
  write.csv(df_orig, tmp, row.names = FALSE)

  df <- import_data(tmp)
  expect_equal(names(df), c("first_name", "last_name", "sale_price"))
})

test_that("import_data makes duplicate column names unique", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  # Manually write CSV with duplicate column names
  writeLines(c("price,price,size", "100,200,50", "300,400,60"), tmp)

  df <- import_data(tmp)
  expect_equal(nrow(df), 2L)
  # Duplicated names should be made unique
  expect_equal(length(unique(names(df))), 3L)
  expect_true("price_1" %in% names(df))
})

test_that("import_data handles empty column names", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c(",value", "x,100", "y,200"), tmp)

  df <- import_data(tmp)
  # Empty column name should become "col"
  expect_true("col" %in% names(df))
})

test_that("import_data warns on zero-row file", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines("a,b,c", tmp)  # headers only, no data rows

  expect_warning(import_data(tmp), "zero rows")
})

test_that("import_data trims leading/trailing underscores", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  df_orig <- data.frame(
    `_price_` = c(100),
    check.names = FALSE
  )
  write.csv(df_orig, tmp, row.names = FALSE)

  df <- import_data(tmp)
  # After non-alphanumeric → _, then trim, should be "price"
  expect_equal(names(df), "price")
})

test_that("import_data handles special characters in column names", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c("Sale $,Lot (acres),Bldg%", "100,0.5,80"), tmp)

  df <- import_data(tmp)
  # Non-alphanumeric chars replaced with _
  expect_true(all(grepl("^[a-z0-9_]+$", names(df))))
})
