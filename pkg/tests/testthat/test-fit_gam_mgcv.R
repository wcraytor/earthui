test_that("fit_gam produces a valid mgcvUI_result", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "hp", type = "s", bs = "tp", k = NULL)
  )

  res <- fit_gam(mtcars, "mpg", specs)

  expect_s3_class(res, "mgcvUI_result")
  expect_s3_class(res$model, "gam")
  expect_equal(res$response, "mpg")
  expect_true(res$elapsed >= 0)
  expect_true(inherits(res$formula, "formula"))
})

test_that("fit_gam works with linear terms", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "cyl", type = "linear", bs = NULL, k = NULL)
  )

  res <- fit_gam(mtcars, "mpg", specs)
  expect_s3_class(res$model, "gam")
})

test_that("fit_gam works with earth knots", {
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

  specs <- list(
    list(vars = "wt", type = "s", bs = "cr", k = NULL),
    list(vars = "hp", type = "s", bs = "cr", k = NULL)
  )

  # reconcile_knots_ will set k to match knot count (or switch to tp)
  res <- fit_gam(mtcars, "mpg", specs, earth_knots = ek)
  expect_s3_class(res$model, "gam")
})

test_that("fit_gam accepts character family", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, family = "gaussian")
  expect_s3_class(res$model, "gam")
})

test_that("format_gam_summary returns expected structure", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs)
  summ <- format_gam_summary(res)

  expect_true(is.numeric(summ$r_squared))
  expect_true(is.numeric(summ$dev_explained))
  expect_true(is.numeric(summ$aic))
  expect_true(is.data.frame(summ$smooth_table))
  expect_true(nrow(summ$smooth_table) > 0)
})

test_that("build_gam_formula produces valid formula", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "cr", k = 5L),
    list(vars = "hp", type = "s", bs = "tp", k = NULL),
    list(vars = "cyl", type = "linear", bs = NULL, k = NULL)
  )
  f <- build_gam_formula("mpg", specs)

  expect_true(inherits(f, "formula"))
  f_str <- deparse(f)
  expect_true(grepl("s\\(wt", f_str))
  expect_true(grepl("s\\(hp", f_str))
  expect_true(grepl("cyl", f_str))
})

test_that("build_gam_formula errors on empty specs", {
  expect_error(build_gam_formula("mpg", list()), "At least one")
})
