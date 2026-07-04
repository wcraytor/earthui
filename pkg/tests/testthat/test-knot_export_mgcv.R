test_that("export_knots_csv_mgcv produces correct format", {
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
  ek <- import_earth_mgcv(er)

  tmp <- tempfile(fileext = ".csv")
  export_knots_csv_mgcv(ek, tmp)

  result <- read.csv(tmp, stringsAsFactors = FALSE)
  unlink(tmp)

  expect_true(is.data.frame(result))
  expect_equal(names(result), c("variable", "knot", "direction"))
  expect_true(all(result$direction %in% c("forward", "reverse")))
  expect_true(is.numeric(result$knot))

  # Every variable with knots should appear
  for (var in names(ek$knots)) {
    expect_true(var %in% result$variable,
                info = paste(var, "should appear in knot CSV"))
  }
})

test_that("export_knots_csv_mgcv rejects non-earth-knots input", {
  expect_error(export_knots_csv_mgcv(list(x = 1), tempfile()),
               "mgcvUI_earth_knots")
})

test_that("export_knots_csv_mgcv handles empty knots", {
  ek <- structure(
    list(
      knots = list(), signs = list(),
      predictors = character(0), target = "y",
      data = data.frame(), earth_summary = list()
    ),
    class = "mgcvUI_earth_knots"
  )

  tmp <- tempfile(fileext = ".csv")
  export_knots_csv_mgcv(ek, tmp)
  result <- read.csv(tmp, stringsAsFactors = FALSE)
  unlink(tmp)

  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("variable", "knot", "direction"))
})
