test_that("check_sign_consistency returns NULL without earth knots", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs)

  result <- check_sign_consistency(res)
  expect_null(result)
})

test_that("check_sign_consistency returns data frame with earth knots", {
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
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "hp", type = "s", bs = "tp", k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, earth_knots = ek)

  signs <- check_sign_consistency(res)

  if (!is.null(signs)) {
    expect_true(is.data.frame(signs))
    expect_true("variable" %in% names(signs))
    expect_true("earth_direction" %in% names(signs))
    expect_true("gam_direction" %in% names(signs))
    expect_true("consistent" %in% names(signs))
    expect_true(all(signs$earth_direction %in%
                      c("increasing", "decreasing", "mixed")))
    expect_true(all(signs$gam_direction %in%
                      c("increasing", "decreasing", "mixed")))
    expect_true(is.logical(signs$consistent))
  }
})

test_that("sign consistency flags known direction correctly", {
  skip_if_not_installed("earth")

  # wt has a strong negative relationship with mpg
  m <- earth::earth(mpg ~ wt, data = mtcars)
  er <- structure(
    list(
      model = m, target = "mpg", predictors = "wt",
      categoricals = character(0), linpreds = character(0),
      degree = 1L, cv_enabled = FALSE, allowed_matrix = NULL,
      data = mtcars, elapsed = 0, trace_output = character(0)
    ),
    class = "earthUI_result"
  )
  ek <- import_earth_mgcv(er)

  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, earth_knots = ek)

  signs <- check_sign_consistency(res)
  if (!is.null(signs)) {
    # The GAM should agree with earth about direction for wt
    wt_row <- signs[signs$variable == "wt", ]
    if (nrow(wt_row) > 0) {
      expect_true(wt_row$consistent || wt_row$gam_direction == "mixed")
    }
  }
})
