test_that("import_data reads CSV files correctly", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  write.csv(mtcars[1:5, ], tmp, row.names = FALSE)

  df <- import_data(tmp)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 5L)
  expect_equal(ncol(df), ncol(mtcars))
  expect_equal(names(df), names(mtcars))
})

test_that("import_data preserves column names with spaces", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  df_orig <- data.frame(
    `Sale Price` = c(100, 200),
    `Lot Size` = c(0.5, 1.0),
    check.names = FALSE
  )
  write.csv(df_orig, tmp, row.names = FALSE)

  df <- import_data(tmp)
  expect_equal(names(df), c("Sale Price", "Lot Size"))
})

test_that("import_data rejects missing file", {
  expect_error(import_data("/nonexistent/file.csv"), "File not found")
})

test_that("import_data rejects unsupported format", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  writeLines("{}", tmp)
  expect_error(import_data(tmp), "Unsupported file format")
})

test_that("import_data rejects non-character filepath", {
  expect_error(import_data(42), "must be a single character string")
})

test_that("import_data reads Excel files", {
  skip_if_not_installed("readxl")
  # Use the example Excel file bundled with readxl
  xlsx_path <- system.file("extdata", "datasets.xlsx", package = "readxl")
  skip_if(xlsx_path == "", "readxl example file not found")

  df <- import_data(xlsx_path, sheet = 1)
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0L)
  expect_gt(ncol(df), 0L)
})
