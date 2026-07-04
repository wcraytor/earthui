test_that("import_data_mgcv reads CSV and cleans names", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  df <- data.frame(`First Name` = "a", `Last.Name` = "b",
                   AGE = 1, check.names = FALSE)
  write.csv(df, tmp, row.names = FALSE)

  result <- import_data_mgcv(tmp)
  expect_true(is.data.frame(result))
  expect_true(all(grepl("^[a-z_]+$", names(result))))
})

test_that("import_data_mgcv errors on unsupported format", {
  tmp <- tempfile(fileext = ".json")
  file.create(tmp)
  on.exit(unlink(tmp))
  expect_error(import_data_mgcv(tmp), "Unsupported file type")
})

test_that("clean_names_ converts camelCase to snake_case", {
  result <- earthUI:::clean_names_(c("firstName", "LastName", "AGE"))
  expect_equal(result, c("first_name", "last_name", "age"))
})

test_that("clean_names_ deduplicates names", {
  result <- earthUI:::clean_names_(c("x", "x", "x"))
  expect_equal(length(unique(result)), 3)
})
