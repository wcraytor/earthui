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
  expect_false("file_basename" %in% cols)  # removed: keyed by (project, purpose)
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
  expect_null(get_project_settings(proj_path, root = root))

  set_project_settings(proj_path,
                       settings  = '{"target":"sale_price"}',
                       variables = '{"a":1}',
                       interactions = "{}",
                       root = root)
  res <- get_project_settings(proj_path, root = root)
  expect_equal(res$settings,  '{"target":"sale_price"}')
  expect_equal(res$variables, '{"a":1}')
  expect_equal(res$interactions, "{}")

  # Update overwrites
  set_project_settings(proj_path,
                       settings  = '{"target":"updated"}',
                       variables = '{"a":2}',
                       interactions = "{}",
                       root = root)
  res2 <- get_project_settings(proj_path, root = root)
  expect_equal(res2$settings, '{"target":"updated"}')
})

test_that("get/set_project_settings scope independently per purpose", {
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  # Same project basename reused under two purposes — settings must not collide
  flat <- "us_tx_aus_p1"
  proj_path <- file.path(root, "mktarea", flat)
  dir.create(proj_path, recursive = TRUE)

  set_project_settings(proj_path, settings = "{}",
                       variables = '{"living":1}', interactions = "{}",
                       purpose = "market", root = root)
  set_project_settings(proj_path, settings = "{}",
                       variables = '{"beds":1}', interactions = "{}",
                       purpose = "general", root = root)

  mkt <- get_project_settings(proj_path,
                              purpose = "market", root = root)
  gen <- get_project_settings(proj_path,
                              purpose = "general", root = root)
  expect_equal(mkt$variables, '{"living":1}')
  expect_equal(gen$variables, '{"beds":1}')

  # A purpose with nothing written is absent (not the other purpose's row)
  expect_null(get_project_settings(proj_path,
                                   purpose = "appraisal", root = root))

  # Default purpose is "general"
  expect_equal(get_project_settings(proj_path, root = root)$variables,
               '{"beds":1}')
})

test_that("set_project_settings supports method = 'glmnet'/'mgcv'", {
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  proj_path <- file.path(root, "appr", "us_ca_081_burlin_test_methods")
  dir.create(proj_path, recursive = TRUE)

  set_project_settings(proj_path,
                       settings  = '{"alpha":0.5}',
                       variables = '{"x":1}',
                       method = "glmnet", root = root)
  res <- get_project_settings(proj_path,
                              method = "glmnet", root = root)
  expect_equal(res$settings, '{"alpha":0.5}')
  # Earth slot should still be empty
  res_earth <- get_project_settings(proj_path,
                                    method = "earth", root = root)
  expect_null(res_earth)
})

test_that("partial write (NULL interactions) preserves existing interactions", {
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  proj_path <- file.path(root, "appr", "us_ca_001_partial_test")
  dir.create(proj_path, recursive = TRUE)

  # Full write incl interactions
  set_project_settings(proj_path,
                       settings     = '{"target":"price"}',
                       variables    = '{"beds":{"inc":true}}',
                       interactions = '{"0_1":true}',
                       purpose = "appraisal", root = root)

  # Partial write: settings + variables only (interactions = NULL).
  # Must update the two supplied columns and LEAVE interactions intact —
  # this is what the server-side fit-time save relies on.
  set_project_settings(proj_path,
                       settings  = '{"target":"rent"}',
                       variables = '{"baths":{"inc":false}}',
                       interactions = NULL,
                       purpose = "appraisal", root = root)

  res <- get_project_settings(proj_path,
                              purpose = "appraisal", root = root)
  expect_equal(res$settings, '{"target":"rent"}')
  expect_equal(res$variables, '{"baths":{"inc":false}}')
  expect_equal(res$interactions, '{"0_1":true}')  # preserved
})

# ---- Regression coverage for today's failure classes -----------------------

test_that("projects DB uses WAL journaling (crash-safe writes)", {
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  con <- regproj_projects_db_connect(root)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  jm <- DBI::dbGetQuery(con, "PRAGMA journal_mode;")[[1]]
  expect_equal(tolower(jm), "wal")
})

test_that("settings saved under one purpose are invisible under another", {
  # The "settings disappeared on reopen" bug: a write under the project's
  # purpose must not be readable under a different purpose, and vice versa.
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  proj <- file.path(root, "appr", "us_ca_001_purpose_iso")
  dir.create(proj, recursive = TRUE)
  set_project_settings(proj, variables = '{"appr":1}',
                       purpose = "appraisal", root = root)
  expect_equal(get_project_settings(proj, purpose = "appraisal",
                                    root = root)$variables, '{"appr":1}')
  expect_null(get_project_settings(proj, purpose = "general", root = root))
  expect_null(get_project_settings(proj, purpose = "market",  root = root))
})

test_that("two different projects keep independent settings", {
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  pa <- file.path(root, "appr", "proj_one"); dir.create(pa, recursive = TRUE)
  pb <- file.path(root, "appr", "proj_two"); dir.create(pb, recursive = TRUE)
  set_project_settings(pa, variables = '{"a":1}', purpose = "appraisal", root = root)
  set_project_settings(pb, variables = '{"b":2}', purpose = "appraisal", root = root)
  expect_equal(get_project_settings(pa, purpose = "appraisal", root = root)$variables,
               '{"a":1}')
  expect_equal(get_project_settings(pb, purpose = "appraisal", root = root)$variables,
               '{"b":2}')
})

test_that("settings are shared across files within a project (no file dimension)", {
  # A project's small test file and full file share one config; the API has no
  # file_basename argument, and the same project row is returned regardless.
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  proj <- file.path(root, "appr", "proj_shared"); dir.create(proj, recursive = TRUE)
  set_project_settings(proj, variables = '{"shared":true}',
                       purpose = "appraisal", root = root)
  expect_equal(get_project_settings(proj, purpose = "appraisal",
                                    root = root)$variables, '{"shared":true}')
})

# --- Migration paths --------------------------------------------------------

# Build a legacy projects.sqlite at `root` and return nothing (closes the con).
.make_legacy_db <- function(root, purpose_col = TRUE, project_purpose = "appr") {
  p <- earthUI:::regproj_projects_db_path(root)
  con <- DBI::dbConnect(RSQLite::SQLite(), p)  # caller inserts rows + disconnects
  DBI::dbExecute(con, "CREATE TABLE projects (flat_segment TEXT PRIMARY KEY, purpose TEXT)")
  DBI::dbExecute(con, sprintf("INSERT INTO projects VALUES ('proj','%s')", project_purpose))
  pcol <- if (purpose_col) "purpose TEXT," else ""
  pk   <- if (purpose_col) "PRIMARY KEY (flat_segment, file_basename, purpose)"
          else             "PRIMARY KEY (flat_segment, file_basename)"
  DBI::dbExecute(con, sprintf("CREATE TABLE project_file_settings (
      flat_segment TEXT, file_basename TEXT, %s
      earth_settings TEXT, earth_variables TEXT, earth_interactions TEXT,
      glmnet_settings TEXT, glmnet_variables TEXT, mgcv_settings TEXT,
      mgcv_variables TEXT, updated_at INTEGER, %s)", pcol, pk))
  con   # returned for caller to insert rows then disconnect
}

test_that("migration (flat,file,purpose) collapses to (flat,purpose), newest kept", {
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  con <- .make_legacy_db(root, purpose_col = TRUE)
  DBI::dbExecute(con, "INSERT INTO project_file_settings VALUES
    ('proj','small.csv','appraisal','{}','{\"v\":\"old\"}','{}',NULL,NULL,NULL,NULL,100)")
  DBI::dbExecute(con, "INSERT INTO project_file_settings VALUES
    ('proj','full.csv','appraisal','{}','{\"v\":\"new\"}','{}',NULL,NULL,NULL,NULL,200)")
  DBI::dbDisconnect(con)

  con2 <- regproj_projects_db_connect(root)
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  cols <- DBI::dbGetQuery(con2, "PRAGMA table_info(project_file_settings)")$name
  expect_false("file_basename" %in% cols)
  expect_false("project_file_settings_old" %in% DBI::dbListTables(con2))
  n <- DBI::dbGetQuery(con2,
    "SELECT COUNT(*) n FROM project_file_settings")$n
  expect_equal(n, 1L)
  expect_equal(earthUI:::project_file_settings_read_(con2, "proj",
               purpose = "appraisal")$variables, '{"v":"new"}')  # updated_at 200
})

test_that("migration of legacy (flat,file) no-purpose schema derives purpose", {
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  con <- .make_legacy_db(root, purpose_col = FALSE, project_purpose = "mktarea")
  DBI::dbExecute(con, "INSERT INTO project_file_settings VALUES
    ('proj','f.csv','{}','{\"x\":1}','{}',NULL,NULL,NULL,NULL,100)")
  DBI::dbDisconnect(con)

  con2 <- regproj_projects_db_connect(root)
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  expect_equal(earthUI:::project_file_settings_read_(con2, "proj",
               purpose = "market")$variables, '{"x":1}')   # mktarea -> market
  expect_null(earthUI:::project_file_settings_read_(con2, "proj", purpose = "general"))
})

test_that("migration is idempotent across reconnects", {
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  con <- .make_legacy_db(root, purpose_col = TRUE, project_purpose = "gen")
  DBI::dbExecute(con, "INSERT INTO project_file_settings VALUES
    ('proj','f.csv','general','{\"t\":1}','{}','{}',NULL,NULL,NULL,NULL,50)")
  DBI::dbDisconnect(con)

  DBI::dbDisconnect(regproj_projects_db_connect(root))   # migrate
  con3 <- regproj_projects_db_connect(root)              # no-op second time
  on.exit(DBI::dbDisconnect(con3), add = TRUE)
  expect_false("project_file_settings_old" %in% DBI::dbListTables(con3))
  expect_equal(earthUI:::project_file_settings_read_(con3, "proj",
               purpose = "general")$settings, '{"t":1}')
})

test_that("interrupted migration leaving only _old is recovered on next connect", {
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  p <- earthUI:::regproj_projects_db_path(root)
  con <- DBI::dbConnect(RSQLite::SQLite(), p)
  DBI::dbExecute(con, "CREATE TABLE projects (flat_segment TEXT PRIMARY KEY, purpose TEXT)")
  DBI::dbExecute(con, "INSERT INTO projects VALUES ('proj','appr')")
  # Simulate a crash AFTER rename but BEFORE the new table was rebuilt:
  # data lives only in project_file_settings_old, no main table.
  DBI::dbExecute(con, "CREATE TABLE project_file_settings_old (
    flat_segment TEXT, file_basename TEXT, purpose TEXT,
    earth_settings TEXT, earth_variables TEXT, earth_interactions TEXT,
    glmnet_settings TEXT, glmnet_variables TEXT, mgcv_settings TEXT,
    mgcv_variables TEXT, updated_at INTEGER,
    PRIMARY KEY (flat_segment, file_basename, purpose))")
  DBI::dbExecute(con, "INSERT INTO project_file_settings_old VALUES
    ('proj','f.csv','appraisal','{}','{\"recovered\":true}','{}',NULL,NULL,NULL,NULL,9)")
  DBI::dbDisconnect(con)

  con2 <- regproj_projects_db_connect(root)
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  expect_false("project_file_settings_old" %in% DBI::dbListTables(con2))
  expect_equal(earthUI:::project_file_settings_read_(con2, "proj",
               purpose = "appraisal")$variables, '{"recovered":true}')
})

test_that("register_project inserts and upserts a project row", {
  root <- new_temp_root()
  proj <- file.path(root, "appr", "us_ca_081_burlin_MyProj")
  dir.create(proj, recursive = TRUE)

  flat <- register_project(proj, "appr", "US", c("ca", "081", "burlin"),
                           "MyProj", last_file = "sales.csv", root = root)
  expect_equal(flat, "us_ca_081_burlin_MyProj")

  con <- regproj_projects_db_connect(root)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  row <- DBI::dbGetQuery(con, "SELECT * FROM projects WHERE flat_segment = ?",
                         params = list(flat))
  expect_equal(nrow(row), 1L)
  expect_equal(row$country, "us")   # lowercased
  expect_equal(row$state, "ca")
  expect_equal(row$county, "081")
  expect_equal(row$city, "burlin")
  expect_equal(row$project_name, "MyProj")
  expect_equal(row$last_file, "sales.csv")
  created_first <- row$created_at

  # Upsert: created_at survives, last_used_at refreshes, NULL last_file
  # preserves the existing value
  register_project(proj, "appr", "US", c("ca", "081", "burlin"),
                   "MyProj", last_file = NULL, root = root)
  row2 <- DBI::dbGetQuery(con, "SELECT * FROM projects WHERE flat_segment = ?",
                          params = list(flat))
  expect_equal(nrow(row2), 1L)
  expect_equal(row2$created_at, created_first)
  expect_equal(row2$last_file, "sales.csv")
})
