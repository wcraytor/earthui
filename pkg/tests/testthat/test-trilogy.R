skip_if_not_installed("jsonlite")

# trilogy.json coordination helpers. All use a tempdir project so the real
# ~/regProj is never touched.
new_proj <- function() {
  d <- tempfile("trilogy_proj_")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

test_that("trilogy_json_path points under the project's trilogy/ folder", {
  proj <- new_proj()
  expect_equal(trilogy_json_path(proj),
               file.path(proj, "trilogy", "trilogy.json"))
})

test_that("trilogy_read returns an unlocked skeleton when absent", {
  proj <- new_proj(); on.exit(unlink(proj, recursive = TRUE), add = TRUE)
  t <- trilogy_read(proj)
  expect_equal(t$schema, 2L)
  expect_false(isTRUE(t$earth_lock$locked))
  expect_false(trilogy_is_locked(proj))
})

test_that("write/read round-trips and creates trilogy/", {
  proj <- new_proj(); on.exit(unlink(proj, recursive = TRUE), add = TRUE)
  t <- trilogy_read(proj)
  t$shared <- list(target = "sale_price", cqa_mode = "per_sf")
  trilogy_write(proj, t)
  expect_true(file.exists(trilogy_json_path(proj)))
  back <- trilogy_read(proj)
  expect_equal(back$shared$target, "sale_price")
  expect_equal(back$shared$cqa_mode, "per_sf")
})

test_that("set_lock records the earth model + downstream + shared; get/is reflect it", {
  proj <- new_proj(); on.exit(unlink(proj, recursive = TRUE), add = TRUE)
  trilogy_set_lock(
    proj, fit_ts = "20260613_101500",
    rds = "mac_out_earth/x_earthUI_result_20260613_101500.rds",
    downstream_locked = list(predictors = list("living_area", "lot_size")),
    shared = list(target = "sale_price", cqa_mode = "raw"))

  expect_true(trilogy_is_locked(proj))
  lock <- trilogy_get_lock(proj)
  expect_true(lock$locked)
  expect_equal(lock$fit_ts, "20260613_101500")
  expect_match(lock$rds, "20260613_101500\\.rds$")
  expect_equal(lock$downstream_locked$predictors[[1]], "living_area")
  expect_equal(trilogy_read(proj)$shared$cqa_mode, "raw")
})

test_that("clear_lock unlocks but leaves other fields", {
  proj <- new_proj(); on.exit(unlink(proj, recursive = TRUE), add = TRUE)
  trilogy_set_lock(proj, "20260613_101500", "x.rds",
                   shared = list(target = "sale_price"))
  trilogy_clear_lock(proj)
  expect_false(trilogy_is_locked(proj))
  expect_equal(trilogy_read(proj)$shared$target, "sale_price")  # preserved
})

test_that("register_fit records per-method stamps and validates method", {
  proj <- new_proj(); on.exit(unlink(proj, recursive = TRUE), add = TRUE)
  trilogy_register_fit(proj, "earth",  "20260613_101500")
  trilogy_register_fit(proj, "glmnet", "20260613_104012")
  trilogy_register_fit(proj, "mgcv",   "20260613_105530")
  ft <- trilogy_read(proj)$run$fit_ts
  expect_equal(ft$earth,  "20260613_101500")
  expect_equal(ft$glmnet, "20260613_104012")
  expect_equal(ft$mgcv,   "20260613_105530")
  expect_error(trilogy_register_fit(proj, "bogus", "x"))
})

test_that("a corrupt trilogy.json falls back to the skeleton (no error)", {
  proj <- new_proj(); on.exit(unlink(proj, recursive = TRUE), add = TRUE)
  p <- trilogy_json_path(proj)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  writeLines("{ this is not valid json", p)
  expect_silent(t <- trilogy_read(proj))
  expect_false(isTRUE(t$earth_lock$locked))
})
