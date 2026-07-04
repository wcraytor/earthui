local_test_settings_db()

test_that("settings_db round-trips data", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  fname <- paste0("test_", as.numeric(Sys.time()), ".csv")

  settings <- list(
    response = "price",
    variables = list(sqft = list(inc = TRUE, type = "numeric", linear = FALSE)),
    family = "gaussian",
    method = "REML",
    select = FALSE,
    gamma = 1.5
  )

  earthUI:::settings_db_write__mgcv(fname, settings)
  result <- earthUI:::settings_db_read__mgcv(fname)

  expect_equal(result$response, "price")
  expect_equal(result$family, "gaussian")
  expect_equal(result$gamma, 1.5)
  expect_true(result$variables$sqft$inc)
})

test_that("settings_db_read__mgcv returns NULL for unknown file", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  result <- earthUI:::settings_db_read__mgcv("nonexistent_file_xyz.csv")
  expect_null(result)
})
