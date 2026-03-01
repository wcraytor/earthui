# Helper: create schema in a connection (mirrors settings_db_connect_ logic)
create_schema_ <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS file_settings (
      filename     TEXT PRIMARY KEY,
      settings     TEXT NOT NULL DEFAULT '{}',
      variables    TEXT NOT NULL DEFAULT '{}',
      interactions TEXT NOT NULL DEFAULT '{}',
      accessed_at  TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%f','now'))
    )
  ")
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_accessed ON file_settings(accessed_at)
  ")
}

test_that("settings_db round-trips correctly", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  create_schema_(con)

  # Write
  earthui:::settings_db_write_(con, "test.csv",
    settings     = '{"degree":"2","target":"price"}',
    variables    = '{"beds":{"inc":true,"fac":false,"lin":false}}',
    interactions = '{"1_2":true}')

  # Read
  result <- earthui:::settings_db_read_(con, "test.csv")
  expect_type(result, "list")
  expect_equal(result$settings$degree, "2")
  expect_equal(result$settings$target, "price")
  expect_true(result$variables$beds$inc)
  expect_false(result$variables$beds$fac)
  expect_true(result$interactions[["1_2"]])
})

test_that("settings_db upsert overwrites existing", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  create_schema_(con)

  earthui:::settings_db_write_(con, "data.csv",
    settings = '{"degree":"1"}',
    variables = '{}', interactions = '{}')

  earthui:::settings_db_write_(con, "data.csv",
    settings = '{"degree":"3"}',
    variables = '{}', interactions = '{}')

  result <- earthui:::settings_db_read_(con, "data.csv")
  expect_equal(result$settings$degree, "3")

  count <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM file_settings")$n
  expect_equal(count, 1L)
})

test_that("LRU eviction keeps only max_files entries", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  create_schema_(con)

  # Insert 5 files with distinct timestamps

  for (i in 1:5) {
    earthui:::settings_db_write_(con, paste0("file", i, ".csv"),
      settings = "{}", variables = "{}", interactions = "{}")
    Sys.sleep(0.05)
  }

  earthui:::settings_db_evict_(con, max_files = 3L)

  count <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM file_settings")$n
  expect_equal(count, 3L)

  remaining <- DBI::dbGetQuery(con,
    "SELECT filename FROM file_settings ORDER BY accessed_at DESC")
  expect_true("file5.csv" %in% remaining$filename)
  expect_true("file4.csv" %in% remaining$filename)
  expect_true("file3.csv" %in% remaining$filename)
})

test_that("earthui:::settings_db_read_ returns NULL for unknown file", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  create_schema_(con)

  result <- earthui:::settings_db_read_(con, "nonexistent.csv")
  expect_null(result)
})

test_that("settings_db functions handle NULL connection gracefully", {
  expect_null(earthui:::settings_db_read_(NULL, "test.csv"))
  expect_invisible(earthui:::settings_db_write_(NULL, "test.csv"))
  expect_invisible(earthui:::settings_db_evict_(NULL))
  expect_invisible(earthui:::settings_db_disconnect_(NULL))
})
