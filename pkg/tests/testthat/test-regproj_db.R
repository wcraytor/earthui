skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

# Use a fresh tempdir as REGPROJ_ROOT for each test
new_temp_root <- function() {
  d <- tempfile("regproj_db_test_")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

test_that("regproj_geo_db_connect creates schema + seeds shipped data", {
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  con <- regproj_geo_db_connect(root)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_setequal(DBI::dbListTables(con),
                  c("countries", "admin_entries"))
  # countries seeded
  n_c <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM countries")$n
  expect_gte(n_c, 24L)
  # US states + counties seeded
  n_us_states <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM admin_entries WHERE country='us' AND level=1")$n
  expect_gte(n_us_states, 51L)
  n_us_counties <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM admin_entries WHERE country='us' AND level=2")$n
  expect_gte(n_us_counties, 3000L)
})

test_that("regproj_index_get/put round-trip via SQLite", {
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  # Force seed by connecting once
  DBI::dbDisconnect(regproj_geo_db_connect(root))

  # Lookup of shipped data
  expect_equal(regproj_index_get("us", "California", root = root), "ca")
  expect_equal(regproj_index_get("us/ca", "Alameda",   root = root), "001")

  # Put a new entry and read back
  regproj_index_put("us/ca/081", "Foster City", "fostr", root = root)
  expect_equal(regproj_index_get("us/ca/081", "Foster City", root = root),
               "fostr")

  # Unknown lookup returns NULL
  expect_null(regproj_index_get("us/ca/081", "Atlantis", root = root))
})

test_that("regproj_index_read returns nested list for legacy callers", {
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  DBI::dbDisconnect(regproj_geo_db_connect(root))
  idx <- regproj_index_read(root)
  expect_true("us" %in% names(idx))
  expect_true("us/ca" %in% names(idx))
  expect_true("California" %in% names(idx[["us"]]))
})

test_that("regproj_geo_db_seed migrates legacy .regproj-index.json", {
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  # Drop a legacy JSON before any DB connect
  legacy <- list("us/ca/081" = list("Belmont" = "belmon",
                                     "Burlingame" = "burlin"))
  jsonlite::write_json(legacy,
                       file.path(root, ".regproj-index.json"),
                       pretty = TRUE, auto_unbox = TRUE)
  DBI::dbDisconnect(regproj_geo_db_connect(root))
  expect_equal(regproj_index_get("us/ca/081", "Belmont",   root = root),
               "belmon")
  expect_equal(regproj_index_get("us/ca/081", "Burlingame", root = root),
               "burlin")
})

test_that("projects DB schema initializes correctly", {
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  con <- regproj_projects_db_connect(root)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_setequal(DBI::dbListTables(con),
                  c("projects", "project_file_settings"))
  cols <- DBI::dbGetQuery(con, "PRAGMA table_info(project_file_settings)")$name
  expect_true("flat_segment" %in% cols)
  expect_true("file_basename" %in% cols)
  expect_true("earth_settings" %in% cols)
  expect_true("glmnet_settings" %in% cols)
  expect_true("mgcv_settings" %in% cols)
})

test_that("get/set_project_settings round-trip", {
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  flat <- "us_ca_081_burlin_test"
  proj_path <- file.path(root, "appr", flat)
  dir.create(proj_path, recursive = TRUE)

  # Initially absent
  expect_null(get_project_settings(proj_path, "data.csv", root = root))

  set_project_settings(proj_path, "data.csv",
                       settings  = '{"target":"sale_price"}',
                       variables = '{"a":1}',
                       interactions = "{}",
                       root = root)
  res <- get_project_settings(proj_path, "data.csv", root = root)
  expect_equal(res$settings,  '{"target":"sale_price"}')
  expect_equal(res$variables, '{"a":1}')
  expect_equal(res$interactions, "{}")

  # Different file in same project — independent
  expect_null(get_project_settings(proj_path, "data_v2.csv", root = root))

  # Update overwrites
  set_project_settings(proj_path, "data.csv",
                       settings  = '{"target":"updated"}',
                       variables = '{"a":2}',
                       interactions = "{}",
                       root = root)
  res2 <- get_project_settings(proj_path, "data.csv", root = root)
  expect_equal(res2$settings, '{"target":"updated"}')
})

test_that("set_project_settings supports method = 'glmnet'/'mgcv'", {
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  proj_path <- file.path(root, "appr", "us_ca_081_burlin_test_methods")
  dir.create(proj_path, recursive = TRUE)

  set_project_settings(proj_path, "data.csv",
                       settings  = '{"alpha":0.5}',
                       variables = '{"x":1}',
                       method = "glmnet", root = root)
  res <- get_project_settings(proj_path, "data.csv",
                              method = "glmnet", root = root)
  expect_equal(res$settings, '{"alpha":0.5}')
  # Earth slot should still be empty
  res_earth <- get_project_settings(proj_path, "data.csv",
                                    method = "earth", root = root)
  expect_null(res_earth)
})
