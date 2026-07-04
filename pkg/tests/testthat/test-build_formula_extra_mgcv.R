# test-build_formula_extra.R — Additional formula building tests

test_that("build_gam_formula with single smooth term", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  f <- build_gam_formula("mpg", specs)
  expect_true(inherits(f, "formula"))
  expect_true(grepl("s\\(wt", deparse(f)))
})

test_that("build_gam_formula with multiple smooth terms", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "hp", type = "s", bs = "cr", k = 5L),
    list(vars = "disp", type = "s", bs = "tp", k = NULL)
  )
  f <- build_gam_formula("mpg", specs)
  f_str <- deparse(f)
  expect_true(grepl("s\\(wt", f_str))
  expect_true(grepl("s\\(hp", f_str))
  expect_true(grepl("s\\(disp", f_str))
})

test_that("build_gam_formula with mixed smooth and linear", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "cyl", type = "linear", bs = NULL, k = NULL),
    list(vars = "am", type = "linear", bs = NULL, k = NULL)
  )
  f <- build_gam_formula("mpg", specs)
  f_str <- deparse(f)
  expect_true(grepl("cyl", f_str))
  expect_true(grepl("am", f_str))
})

test_that("build_gam_formula with tensor product", {
  specs <- list(
    list(vars = c("wt", "hp"), type = "te", bs = NULL, k = 5L)
  )
  f <- build_gam_formula("mpg", specs)
  f_str <- deparse(f)
  expect_true(grepl("te\\(wt", f_str))
  expect_true(grepl("hp", f_str))
})

test_that("build_gam_formula with k specified", {
  specs <- list(list(vars = "wt", type = "s", bs = "cr", k = 8L))
  f <- build_gam_formula("mpg", specs)
  f_str <- deparse(f)
  expect_true(grepl("k = 8", f_str))
})

test_that("build_gam_formula with only linear terms", {
  specs <- list(
    list(vars = "wt", type = "linear", bs = NULL, k = NULL),
    list(vars = "hp", type = "linear", bs = NULL, k = NULL)
  )
  f <- build_gam_formula("mpg", specs)
  f_str <- deparse(f)
  expect_true(grepl("wt", f_str))
  expect_true(grepl("hp", f_str))
  expect_false(grepl("s\\(", f_str))
})

test_that("format_smooth_term_ with bs=ps", {
  result <- earthUI:::format_smooth_term_(
    list(vars = "x", type = "s", bs = "ps", k = 10L)
  )
  expect_equal(result, "s(x, bs = \"ps\", k = 10)")
})

test_that("format_smooth_term_ with bs=bs", {
  result <- earthUI:::format_smooth_term_(
    list(vars = "x", type = "s", bs = "bs", k = NULL)
  )
  expect_equal(result, "s(x, bs = \"bs\")")
})

test_that("build_gam_knots returns NULL without earth knots", {
  specs <- list(list(vars = "wt", type = "s", bs = "cr", k = NULL))
  result <- build_gam_knots(specs, NULL)
  expect_null(result)
})

test_that("build_gam_knots with empty specs", {
  expect_null(build_gam_knots(list(), NULL))
})
