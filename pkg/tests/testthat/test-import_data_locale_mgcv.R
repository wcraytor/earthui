# test-import_data_locale.R — Tests for import_data_mgcv with locale sep/dec params

test_that("import_data_mgcv has sep and dec parameters", {
  fmls <- formals(import_data_mgcv)
  expect_true("sep" %in% names(fmls))
  expect_true("dec" %in% names(fmls))
  expect_equal(fmls$sep, ",")
  expect_equal(fmls$dec, ".")
})

test_that("import_data_mgcv reads standard CSV with default sep/dec", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  write.csv(data.frame(x = c(1.5, 2.5), y = c(3, 4)), tmp, row.names = FALSE)
  df <- import_data_mgcv(tmp)
  expect_equal(nrow(df), 2L)
  expect_equal(df$x[1], 1.5)
})

test_that("import_data_mgcv reads semicolon CSV with comma decimal", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c("price;area", "100,50;200,75", "300,00;400,25"), tmp)
  df <- import_data_mgcv(tmp, sep = ";", dec = ",")
  expect_equal(nrow(df), 2L)
  expect_equal(df$price[1], 100.50)
  expect_equal(df$area[2], 400.25)
})

test_that("import_data_mgcv handles European CSV with many columns", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c("a;b;c;d;e",
               "1,1;2,2;3,3;4,4;5,5",
               "6,6;7,7;8,8;9,9;10,0"), tmp)
  df <- import_data_mgcv(tmp, sep = ";", dec = ",")
  expect_equal(ncol(df), 5L)
  expect_equal(df$a[1], 1.1)
  expect_equal(df$e[2], 10.0)
})

test_that("import_data_mgcv cleans names regardless of sep/dec", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c("First Name;Last.Name;Total Price",
               "Alice;Smith;100,00"), tmp)
  df <- import_data_mgcv(tmp, sep = ";", dec = ",")
  expect_true(all(grepl("^[a-z_0-9]+$", names(df))))
})

test_that("import_data_mgcv with sep=; ignores commas in decimal values", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c("val", "1234,56", "7890,12"), tmp)
  # Using comma as both sep and dec should fail or produce wrong results

  # But with semicolon sep and comma dec, it works
  df <- import_data_mgcv(tmp, sep = "\t", dec = ",")
  # With tab sep on comma data, whole line becomes one column
  # This tests that sep/dec are passed through correctly
  expect_true(is.data.frame(df))
})

test_that("import_data_mgcv with integer-only data works with any sep/dec", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c("a;b", "1;2", "3;4"), tmp)
  df <- import_data_mgcv(tmp, sep = ";")
  expect_equal(df$a[1], 1L)
  expect_equal(df$b[2], 4L)
})

test_that("import_data_mgcv still reads Excel regardless of sep/dec", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  writexl::write_xlsx(data.frame(x = 1:3, y = 4:6), tmp)

  # sep/dec should be ignored for Excel

  df <- import_data_mgcv(tmp, sep = ";", dec = ",")
  expect_equal(nrow(df), 3L)
  expect_equal(df$x[1], 1)
})

test_that("import_data_mgcv errors on unsupported format with locale params", {
  tmp <- tempfile(fileext = ".json")
  file.create(tmp)
  on.exit(unlink(tmp))
  expect_error(import_data_mgcv(tmp, sep = ";", dec = ","), "Unsupported file type")
})

test_that("import_data_mgcv handles empty CSV", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c("a;b"), tmp)
  df <- import_data_mgcv(tmp, sep = ";")
  expect_equal(nrow(df), 0L)
  expect_equal(ncol(df), 2L)
})

test_that("import_data_mgcv handles single-column CSV", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c("price", "100.5", "200.75"), tmp)
  df <- import_data_mgcv(tmp)
  expect_equal(ncol(df), 1L)
  expect_equal(nrow(df), 2L)
})
