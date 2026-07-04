# test-export_report.R — Tests for report rendering and export

test_that("render_gam_report has expected parameters", {
  fmls <- formals(render_gam_report)
  expect_true("gam_result" %in% names(fmls))
  expect_true("output_format" %in% names(fmls))
  expect_true("output_file" %in% names(fmls))
  expect_true("title" %in% names(fmls))
  expect_equal(fmls$output_format, "html")
})

test_that("render_gam_report errors on unsupported format", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  expect_error(render_gam_report(res, "txt"), "Unsupported format")
})

test_that("render_gam_report template exists", {
  tmpl <- system.file("rmd", "gam_report.Rmd", package = "earthUI")
  expect_true(nzchar(tmpl))
  expect_true(file.exists(tmpl))
})

test_that("export_gam_docx produces a Word file", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  tmp <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp))
  result <- export_gam_docx(res, tmp)

  expect_true(file.exists(tmp))
  expect_true(file.size(tmp) > 0)
  expect_equal(result, tmp)
})

test_that("export_gam_docx with linear terms", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "cyl", type = "linear", bs = NULL, k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  tmp <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp))
  result <- export_gam_docx(res, tmp)
  expect_true(file.exists(tmp))
})

test_that("render_gam_report generates HTML", {
  skip_on_cran()
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp))
  result <- render_gam_report(res, "html", output_file = tmp)

  expect_true(file.exists(tmp))
  expect_true(file.size(tmp) > 0)
})
