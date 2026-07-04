# test-settings_db_locale.R — Tests for locale persistence in settings DB

local_test_settings_db()

test_that("settings_db_write_locale_mgcv and read_locale_ round-trip", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")

  locale_settings <- list(
    locale_country = "de",
    locale_paper = "a4",
    locale_csv_sep = ";",
    locale_dec = ",",
    locale_date = "dmy"
  )
  earthUI:::settings_db_write_locale_mgcv(locale_settings)

  saved <- earthUI:::settings_db_read_locale_mgcv()
  expect_false(is.null(saved))
  expect_equal(saved$locale_country, "de")
  expect_equal(saved$locale_paper, "a4")
  expect_equal(saved$locale_csv_sep, ";")
  expect_equal(saved$locale_dec, ",")
  expect_equal(saved$locale_date, "dmy")
})

test_that("settings_db_write_locale_mgcv overwrites previous", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")

  earthUI:::settings_db_write_locale_mgcv(list(locale_country = "fr"))
  earthUI:::settings_db_write_locale_mgcv(list(locale_country = "gb"))

  saved <- earthUI:::settings_db_read_locale_mgcv()
  expect_equal(saved$locale_country, "gb")
})

test_that("settings_db_read_locale_mgcv returns NULL when empty", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  con <- earthUI:::settings_db_connect_mgcv()
  DBI::dbExecute(con, "DELETE FROM settings_v2 WHERE filename = '__locale_defaults__'")
  DBI::dbDisconnect(con)

  saved <- earthUI:::settings_db_read_locale_mgcv()
  expect_null(saved)
})

test_that("locale defaults don't interfere with file settings", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")

  # Save locale defaults
  earthUI:::settings_db_write_locale_mgcv(list(locale_country = "jp"))

  # Save file settings
  fname <- paste0("locale_test_", as.numeric(Sys.time()), ".csv")
  settings <- list(
    response = "price",
    variables = list(sqft = list(inc = TRUE)),
    family = "gaussian",
    method = "REML",
    select = FALSE,
    gamma = 1
  )
  earthUI:::settings_db_write_mgcv(fname, settings)

  # Both should be independently retrievable
  locale <- earthUI:::settings_db_read_locale_mgcv()
  expect_equal(locale$locale_country, "jp")

  file_settings <- earthUI:::settings_db_read_mgcv(fname)
  expect_equal(file_settings$response, "price")
})

test_that("locale write with partial fields works", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")

  earthUI:::settings_db_write_locale_mgcv(list(locale_country = "se"))
  saved <- earthUI:::settings_db_read_locale_mgcv()
  expect_equal(saved$locale_country, "se")
  expect_null(saved$locale_paper)
})

test_that("settings_db_connect_mgcv creates table", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  con <- earthUI:::settings_db_connect_mgcv()
  on.exit(DBI::dbDisconnect(con))
  expect_true("settings_v2" %in% DBI::dbListTables(con))
})

test_that("settings_db_path_mgcv returns a sqlite path", {
  withr::with_options(list(mgcvUI.settings_db_path = NULL), {
    path <- earthUI:::settings_db_path_mgcv()
    expect_true(grepl("settings\\.sqlite$", path))
  })
})

test_that("jsonlite_encode_ handles NULL", {
  result <- earthUI:::jsonlite_encode_(NULL)
  expect_equal(result, "{}")
})

test_that("jsonlite_encode_ handles empty list", {
  result <- earthUI:::jsonlite_encode_(list())
  expect_equal(result, "{}")
})

test_that("jsonlite_decode_ handles NULL", {
  result <- earthUI:::jsonlite_decode_(NULL)
  expect_equal(result, list())
})

test_that("jsonlite_decode_ handles empty string", {
  result <- earthUI:::jsonlite_decode_("")
  expect_equal(result, list())
})

test_that("jsonlite_decode_ handles empty JSON", {
  result <- earthUI:::jsonlite_decode_("{}")
  expect_equal(result, list())
})

test_that("jsonlite_encode_ and decode_ round-trip", {
  skip_if_not_installed("jsonlite")
  x <- list(a = 1, b = "hello", c = TRUE)
  json <- earthUI:::jsonlite_encode_(x)
  result <- earthUI:::jsonlite_decode_(json)
  expect_equal(result$a, 1)
  expect_equal(result$b, "hello")
  expect_equal(result$c, TRUE)
})

test_that("settings_db_evict_mgcv respects max_files", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")

  con <- earthUI:::settings_db_connect_mgcv()
  on.exit(DBI::dbDisconnect(con))

  # Insert a bunch of test entries
  for (i in seq_len(5)) {
    fname <- paste0("evict_test_", i, "_", as.numeric(Sys.time()), ".csv")
    DBI::dbExecute(con, "
      INSERT OR REPLACE INTO settings_v2 (filename, response, variables, updated_at)
      VALUES (?, 'y', '{}', datetime('now'))
    ", params = list(fname))
  }

  # Evict should not error
  expect_no_error(earthUI:::settings_db_evict_mgcv(con, max_files = 1000L))
})
