test_that("default_regproj_root respects REGPROJ_ROOT env var", {
  withr::local_envvar(REGPROJ_ROOT = "/some/custom/path")
  # path.expand on absolute path is a no-op
  expect_identical(default_regproj_root(), "/some/custom/path")
})

test_that("default_regproj_root falls back to OS default when env unset", {
  withr::local_envvar(REGPROJ_ROOT = "")
  # Use a sandboxed prefs path so we don't read real user prefs
  withr::local_envvar(R_USER_DATA_DIR = tempfile("rud_"),
                      R_USER_CONFIG_DIR = tempfile("rcd_"))
  rt <- default_regproj_root()
  if (.Platform$OS.type == "windows") {
    expect_identical(rt, "C:/regProj")
  } else {
    expect_identical(rt, path.expand("~/regProj"))
  }
})

test_that("earthui_prefs round-trip works", {
  withr::local_envvar(R_USER_CONFIG_DIR = tempfile("prefs_test_"))
  expect_identical(earthui_prefs_read(), list())
  earthui_prefs_write(list(regproj_root = "/Volumes/foo/regProj",
                           extra = "value"))
  prefs <- earthui_prefs_read()
  expect_equal(prefs$regproj_root, "/Volumes/foo/regProj")
  expect_equal(prefs$extra, "value")
})

test_that("default_regproj_root reads regproj_root from prefs", {
  cfg_dir <- tempfile("prefs_test_")
  withr::local_envvar(REGPROJ_ROOT = "",
                      R_USER_CONFIG_DIR = cfg_dir)
  earthui_prefs_write(list(regproj_root = "/saved/from/prefs"))
  expect_identical(default_regproj_root(), "/saved/from/prefs")
})

test_that("env var takes precedence over prefs", {
  cfg_dir <- tempfile("prefs_test_")
  withr::local_envvar(REGPROJ_ROOT = "/env/wins",
                      R_USER_CONFIG_DIR = cfg_dir)
  earthui_prefs_write(list(regproj_root = "/prefs/loses"))
  expect_identical(default_regproj_root(), "/env/wins")
})

test_that("regproj_in_files lists files in <project>/<os>_in/", {
  root <- tempfile("inf_test_")
  in_dir <- file.path(root, "mac_in")
  dir.create(in_dir, recursive = TRUE)
  writeLines("hi", file.path(in_dir, "data.csv"))
  writeLines("hi", file.path(in_dir, "extra.csv"))
  writeLines("hidden", file.path(in_dir, ".regproj-last"))

  files <- regproj_in_files(root, os = "mac")
  expect_setequal(files, c("data.csv", "extra.csv"))  # dotfile excluded
})

test_that("regproj_last_file marker round-trips", {
  root <- tempfile("lastf_test_")
  dir.create(root, recursive = TRUE)
  expect_null(regproj_last_file_get(root, os = "mac"))
  regproj_last_file_set(root, "data_v2.csv", os = "mac")
  expect_identical(regproj_last_file_get(root, os = "mac"), "data_v2.csv")
})

test_that("regproj_list_projects walks the flat tree", {
  root <- tempfile("listproj_test_")
  dir.create(root, recursive = TRUE)
  # Two projects under appr, one under gen
  for (flat in c("us_ca_081_burlin_proj_a", "us_ca_075_sanfra_proj_b")) {
    dir.create(file.path(root, "appr", flat), recursive = TRUE)
  }
  dir.create(file.path(root, "gen", "us_ca_001_oaklan_misc"),
             recursive = TRUE)
  # Bogus dir at wrong depth — should be ignored
  dir.create(file.path(root, "appr", "not-a-project"), recursive = TRUE)

  df <- regproj_list_projects(root = root)
  expect_equal(nrow(df), 3L)
  expect_setequal(df$project_name,
                  c("proj_a", "proj_b", "misc"))
  expect_true(all(df$country == "us"))
})
