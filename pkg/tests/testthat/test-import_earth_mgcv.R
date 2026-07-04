test_that("import_earth_mgcv extracts knots from earth model", {
  skip_if_not_installed("earth")

  m <- earth::earth(mpg ~ wt + hp + disp, data = mtcars)
  er <- structure(
    list(
      model = m,
      target = "mpg",
      predictors = c("wt", "hp", "disp"),
      categoricals = character(0),
      linpreds = character(0),
      degree = 1L,
      cv_enabled = FALSE,
      allowed_matrix = NULL,
      data = mtcars,
      elapsed = 0,
      trace_output = character(0)
    ),
    class = "earthUI_result"
  )

  ek <- import_earth_mgcv(er)

  expect_s3_class(ek, "mgcvUI_earth_knots")
  expect_true(is.list(ek$knots))
  expect_true(is.list(ek$signs))
  expect_equal(ek$target, "mpg")
  expect_equal(ek$predictors, c("wt", "hp", "disp"))

  # Each knot vector should be numeric

  for (var in names(ek$knots)) {
    expect_true(is.numeric(ek$knots[[var]]))
    expect_true(is.integer(ek$signs[[var]]))
    # Signs should be 1 or -1
    expect_true(all(ek$signs[[var]] %in% c(1L, -1L)))
    # Knots should be sorted
    expect_equal(ek$knots[[var]], sort(ek$knots[[var]]))
  }
})

test_that("import_earth_mgcv works from .rds file", {
  skip_if_not_installed("earth")

  m <- earth::earth(mpg ~ wt + hp, data = mtcars)
  er <- structure(
    list(
      model = m, target = "mpg",
      predictors = c("wt", "hp"),
      categoricals = character(0), linpreds = character(0),
      degree = 1L, cv_enabled = FALSE, allowed_matrix = NULL,
      data = mtcars, elapsed = 0, trace_output = character(0)
    ),
    class = "earthUI_result"
  )

  tmp <- tempfile(fileext = ".rds")
  saveRDS(er, tmp)
  ek <- import_earth_mgcv(tmp)
  unlink(tmp)

  expect_s3_class(ek, "mgcvUI_earth_knots")
})

test_that("import_earth_mgcv rejects non-earthUI objects", {
  expect_error(import_earth_mgcv(list(x = 1)), "earthUI_result")
})

test_that("import_earth_mgcv rejects missing files", {
  expect_error(import_earth_mgcv("/nonexistent/file.rds"), "File not found")
})
