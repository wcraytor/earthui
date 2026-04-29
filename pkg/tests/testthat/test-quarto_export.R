skip_if_not_installed("earth")

# Tiny earth fit just to feed the asset prep
make_tiny_result <- function() {
  fit_earth(mtcars[, c("mpg", "cyl", "disp", "hp", "wt")],
            target = "mpg",
            predictors = c("cyl", "disp", "hp", "wt"),
            degree = 1L, .capture_trace = FALSE)
}

test_that("generate_quarto_report writes a self-contained bundle", {
  skip_if_not(file.exists(system.file("quarto", "earth_report.qmd",
                                       package = "earthUI")))
  result <- make_tiny_result()
  dest <- tempfile("qmd_bundle_")
  dir.create(dest, recursive = TRUE)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  qmd <- generate_quarto_report(result, dest_dir = dest, base = "test")
  expect_true(file.exists(qmd))
  expect_match(qmd, "test\\.qmd$")

  bundle_dir <- dirname(qmd)
  expect_true(file.exists(file.path(bundle_dir, "report_data.rds")))
  expect_true(dir.exists(file.path(bundle_dir, "plots")))

  # report_data.rds is xz-compressed and lean (no full earth_model object)
  size_kb <- file.size(file.path(bundle_dir, "report_data.rds")) / 1024
  expect_lt(size_kb, 5000)   # always under 5 MB even on tiny fits

  # Lean result has only metadata fields, no $model
  data <- readRDS(file.path(bundle_dir, "report_data.rds"))
  expect_null(data$result$model)
  expect_true("predictors" %in% names(data$result))
  expect_true("degree" %in% names(data$result))
})

test_that("generated qmd has data_file inlined for self-rendering", {
  skip_if_not(file.exists(system.file("quarto", "earth_report.qmd",
                                       package = "earthUI")))
  result <- make_tiny_result()
  dest <- tempfile("qmd_self_")
  dir.create(dest, recursive = TRUE)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  qmd <- generate_quarto_report(result, dest_dir = dest, base = "selftest")
  txt <- readLines(qmd, warn = FALSE)
  # data_file should now point to the local report_data.rds, not be empty
  expect_true(any(grepl('^  data_file: "report_data.rds"$', txt)))
  expect_false(any(grepl('^  data_file: ""$', txt)))
})

test_that("convert_quarto_file errors on missing input gracefully", {
  expect_error(
    convert_quarto_file("/nonexistent/file.qmd", formats = "html"),
    "not found"
  )
})

test_that("convert_quarto_file errors on unknown format", {
  qmd <- tempfile(fileext = ".qmd")
  writeLines("---\ntitle: x\n---\n", qmd)
  on.exit(unlink(qmd))
  expect_error(
    convert_quarto_file(qmd, formats = "epub"),
    "should be one of"
  )
})
