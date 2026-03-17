# Tests for export_report.R: prepare_report_assets() and render_report()

# ---------------------------------------------------------------------------
# render_report() error paths (existing tests)
# ---------------------------------------------------------------------------

test_that("render_report rejects non-earthUI_result", {
  expect_error(render_report(list()), "earthUI_result")
})

test_that("render_report rejects invalid output_format", {
  result <- fit_earth(mtcars, "mpg", c("wt", "hp"))
  expect_error(render_report(result, output_format = "txt"),
               "'arg' should be one of")
})

test_that("render_report errors when quarto package missing", {
  result <- fit_earth(mtcars, "mpg", c("wt", "hp"))
  if (!requireNamespace("quarto", quietly = TRUE)) {
    expect_error(render_report(result, output_format = "html"),
                 "quarto.*package is required")
  }
})

test_that("render_report accepts valid output_format values", {
  result <- fit_earth(mtcars, "mpg", c("wt", "hp"))
  for (fmt in c("html", "pdf", "docx")) {
    err <- tryCatch(render_report(result, output_format = fmt),
                    error = function(e) conditionMessage(e))
    if (is.character(err)) {
      expect_false(grepl("'arg' should be one of", err),
                   info = paste("Format", fmt, "should be accepted"))
    }
  }
})

# ---------------------------------------------------------------------------
# prepare_report_assets() — basic workflow (single response)
# ---------------------------------------------------------------------------

test_that("prepare_report_assets creates assets directory with expected structure", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
  assets_dir <- prepare_report_assets(result)
  on.exit(unlink(assets_dir, recursive = TRUE), add = TRUE)

  expect_true(dir.exists(assets_dir))
  expect_true(file.exists(file.path(assets_dir, "report_data.rds")))
  expect_true(dir.exists(file.path(assets_dir, "plots")))
})

test_that("prepare_report_assets RDS contains expected fields", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
  assets_dir <- prepare_report_assets(result)
  on.exit(unlink(assets_dir, recursive = TRUE), add = TRUE)

  rds <- readRDS(file.path(assets_dir, "report_data.rds"))
  expected_names <- c("result", "summary_info", "equation", "importance",
                      "gf", "anova", "multi", "targets")
  expect_true(all(expected_names %in% names(rds)))
  expect_false(rds$multi)
  expect_equal(rds$targets, "mpg")
  expect_s3_class(rds$result, "earthUI_result")
})

# ---------------------------------------------------------------------------
# prepare_report_assets() — plot generation (single response)
# ---------------------------------------------------------------------------

test_that("prepare_report_assets generates PNG and PDF plots for single-response", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
  assets_dir <- prepare_report_assets(result)
  on.exit(unlink(assets_dir, recursive = TRUE), add = TRUE)

  plots_dir <- file.path(assets_dir, "plots")
  plot_files <- list.files(plots_dir)

  expect_true("importance.png" %in% plot_files)
  expect_true("importance.pdf" %in% plot_files)
  expect_true("correlation.png" %in% plot_files)
  expect_true("correlation.pdf" %in% plot_files)
  expect_true("residuals.png" %in% plot_files)
  expect_true("residuals.pdf" %in% plot_files)
  expect_true("qq.png" %in% plot_files)
  expect_true("qq.pdf" %in% plot_files)
  expect_true("actual_vs_predicted.png" %in% plot_files)
  expect_true("actual_vs_predicted.pdf" %in% plot_files)

  contour_pngs <- grep("^gfunc_.*_contour\\.png$", plot_files, value = TRUE)
  contour_pdfs <- grep("^gfunc_.*_contour\\.pdf$", plot_files, value = TRUE)
  expect_true(length(contour_pngs) > 0)
  expect_true(length(contour_pdfs) > 0)
})

test_that("prepare_report_assets generates persp plots for degree >= 2", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"), degree = 2)
  assets_dir <- prepare_report_assets(result)
  on.exit(unlink(assets_dir, recursive = TRUE), add = TRUE)

  plots_dir <- file.path(assets_dir, "plots")
  rds <- readRDS(file.path(assets_dir, "report_data.rds"))
  gf <- rds$gf

  if (any(gf$d >= 2L)) {
    persp_pngs <- grep("^gfunc_.*_persp\\.png$", list.files(plots_dir),
                       value = TRUE)
    expect_true(length(persp_pngs) > 0)
  }
})

# ---------------------------------------------------------------------------
# prepare_report_assets() — multi-response model
# ---------------------------------------------------------------------------

test_that("prepare_report_assets handles multi-response model", {
  result <- fit_earth(mtcars, c("mpg", "qsec"), c("cyl", "disp", "hp", "wt"))
  assets_dir <- prepare_report_assets(result)
  on.exit(unlink(assets_dir, recursive = TRUE), add = TRUE)

  rds <- readRDS(file.path(assets_dir, "report_data.rds"))
  expect_true(rds$multi)
  expect_equal(rds$targets, c("mpg", "qsec"))

  plots_dir <- file.path(assets_dir, "plots")
  plot_files <- list.files(plots_dir)

  expect_true("residuals_r1.png" %in% plot_files)
  expect_true("residuals_r2.png" %in% plot_files)
  expect_true("qq_r1.png" %in% plot_files)
  expect_true("qq_r2.png" %in% plot_files)

  contour_r1 <- grep("^gfunc_.*_r1_contour\\.png$", plot_files, value = TRUE)
  contour_r2 <- grep("^gfunc_.*_r2_contour\\.png$", plot_files, value = TRUE)
  expect_true(length(contour_r1) > 0)
  expect_true(length(contour_r2) > 0)
})

# ---------------------------------------------------------------------------
# prepare_report_assets() — custom assets_dir
# ---------------------------------------------------------------------------

test_that("prepare_report_assets uses custom assets_dir when provided", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
  custom_dir <- file.path(tempdir(),
    paste0("custom_assets_", format(Sys.time(), "%Y%m%d%H%M%S")))
  on.exit(unlink(custom_dir, recursive = TRUE), add = TRUE)

  returned_dir <- prepare_report_assets(result, assets_dir = custom_dir)

  expect_equal(normalizePath(returned_dir), normalizePath(custom_dir))
  expect_true(file.exists(file.path(custom_dir, "report_data.rds")))
  expect_true(dir.exists(file.path(custom_dir, "plots")))
})

# ---------------------------------------------------------------------------
# prepare_report_assets() — error handling
# ---------------------------------------------------------------------------

test_that("prepare_report_assets rejects non-earthUI_result", {
  expect_error(prepare_report_assets(list()), "earthUI_result")
})

# ---------------------------------------------------------------------------
# render_report() with assets_dir (pre-computed assets)
# ---------------------------------------------------------------------------

test_that("render_report uses provided assets_dir", {
  skip_if_not_installed("quarto")

  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
  assets_dir <- prepare_report_assets(result)
  on.exit(unlink(assets_dir, recursive = TRUE), add = TRUE)

  output_file <- tempfile(fileext = ".html")
  on.exit(unlink(output_file), add = TRUE)

  rendered <- tryCatch(
    render_report(result, output_format = "html",
                  output_file = output_file, assets_dir = assets_dir),
    error = function(e) e
  )

  if (!inherits(rendered, "error")) {
    expect_true(file.exists(output_file))
    expect_true(file.info(output_file)$size > 0)
  }

  # assets_dir should still exist (not cleaned up since we provided it)
  expect_true(dir.exists(assets_dir))
})

test_that("render_report works without assets_dir (backward compat)", {
  skip_if_not_installed("quarto")

  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
  output_file <- tempfile(fileext = ".html")
  on.exit(unlink(output_file), add = TRUE)

  rendered <- tryCatch(
    render_report(result, output_format = "html",
                  output_file = output_file, assets_dir = NULL),
    error = function(e) e
  )

  if (!inherits(rendered, "error")) {
    expect_true(file.exists(output_file))
    expect_true(file.info(output_file)$size > 0)
  }
})

test_that("render_report with assets_dir does not modify the assets directory", {
  skip_if_not_installed("quarto")

  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
  assets_dir <- prepare_report_assets(result)
  on.exit(unlink(assets_dir, recursive = TRUE), add = TRUE)

  files_before <- sort(list.files(assets_dir, recursive = TRUE))

  output_file <- tempfile(fileext = ".html")
  on.exit(unlink(output_file), add = TRUE)

  tryCatch(
    render_report(result, output_format = "html",
                  output_file = output_file, assets_dir = assets_dir),
    error = function(e) NULL
  )

  files_after <- sort(list.files(assets_dir, recursive = TRUE))
  expect_equal(files_before, files_after)
})
