test_that("build_gam_knots returns NULL without earth knots", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  result <- build_gam_knots(specs, NULL)
  expect_null(result)
})

test_that("build_gam_knots only passes knots for cr/ps/bs bases", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "hp", type = "s", bs = "cr", k = 3L)
  )
  ek <- list(
    knots = list(wt = c(2, 3, 4), hp = c(100, 150, 200)),
    signs = list(wt = c(-1L, -1L, -1L), hp = c(1L, 1L, 1L))
  )
  class(ek) <- "mgcvUI_earth_knots"

  result <- build_gam_knots(specs, ek)
  # tp should NOT get knots, cr should
  expect_null(result[["wt"]])
  expect_equal(result[["hp"]], c(100, 150, 200))
})

test_that("format_smooth_term_ handles all term types", {
  # Smooth with basis and k
  expect_equal(
    earthUI:::format_smooth_term_(
      list(vars = "x", type = "s", bs = "cr", k = 5L)
    ),
    "s(x, bs = \"cr\", k = 5)"
  )

  # Smooth without k
  expect_equal(
    earthUI:::format_smooth_term_(
      list(vars = "x", type = "s", bs = "tp", k = NULL)
    ),
    "s(x, bs = \"tp\")"
  )

  # Linear term
  expect_equal(
    earthUI:::format_smooth_term_(
      list(vars = "x", type = "linear", bs = NULL, k = NULL)
    ),
    "x"
  )

  # Tensor product
  expect_equal(
    earthUI:::format_smooth_term_(
      list(vars = c("x", "y"), type = "te", bs = NULL, k = 5L)
    ),
    "te(x, y, k = 5)"
  )
})
