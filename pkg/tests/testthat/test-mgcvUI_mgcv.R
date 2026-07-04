test_that("mgcvUI exists and is a function", {
  expect_true(is.function(launch_mgcv))
})

test_that("mgcvUI has expected default port", {
  expect_equal(formals(launch_mgcv)$port, 7880L)
})
