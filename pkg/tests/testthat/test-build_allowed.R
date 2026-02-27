test_that("build_allowed_matrix creates correct default matrix", {
  mat <- build_allowed_matrix(c("a", "b", "c"))
  expect_true(is.matrix(mat))
  expect_true(is.logical(mat))
  expect_equal(dim(mat), c(3L, 3L))
  expect_equal(rownames(mat), c("a", "b", "c"))
  expect_equal(colnames(mat), c("a", "b", "c"))
  expect_true(all(mat))
})

test_that("build_allowed_matrix supports default = FALSE", {
  mat <- build_allowed_matrix(c("a", "b"), default = FALSE)
  expect_false(any(mat))
})

test_that("build_allowed_matrix rejects empty input", {
  expect_error(build_allowed_matrix(character(0)), "non-empty character vector")
})

test_that("build_allowed_matrix rejects duplicates", {
  expect_error(build_allowed_matrix(c("a", "a")), "duplicates")
})

test_that("build_allowed_function returns a function", {
  mat <- build_allowed_matrix(c("a", "b"))
  func <- build_allowed_function(mat)
  expect_type(func, "closure")
})

test_that("build_allowed_function allows degree 1 always", {
  mat <- build_allowed_matrix(c("a", "b"), default = FALSE)
  func <- build_allowed_function(mat)
  # degree = 1, pred = 1, no parents
  result <- func(degree = 1, pred = 1, parents = c(FALSE, FALSE),
                 namesx = c("a", "b"), first = TRUE)
  expect_true(result)
})

test_that("build_allowed_function blocks disallowed degree 2 interaction", {
  mat <- build_allowed_matrix(c("a", "b", "c"))
  mat["a", "b"] <- FALSE
  mat["b", "a"] <- FALSE
  func <- build_allowed_function(mat)

  # Propose adding "a" (pred=1) to a term that already has "b" (parents[2]=TRUE)
  result <- func(degree = 2, pred = 1, parents = c(FALSE, TRUE, FALSE),
                 namesx = c("a", "b", "c"), first = FALSE)
  expect_false(result)
})

test_that("build_allowed_function allows permitted degree 2 interaction", {
  mat <- build_allowed_matrix(c("a", "b", "c"))
  mat["a", "b"] <- FALSE
  mat["b", "a"] <- FALSE
  func <- build_allowed_function(mat)

  # Propose adding "a" (pred=1) to a term with "c" (parents[3]=TRUE)
  result <- func(degree = 2, pred = 1, parents = c(FALSE, FALSE, TRUE),
                 namesx = c("a", "b", "c"), first = FALSE)
  expect_true(result)
})

test_that("build_allowed_function checks all pairwise for 3-way", {

  mat <- build_allowed_matrix(c("a", "b", "c"))
  # Block only a-c

  mat["a", "c"] <- FALSE
  mat["c", "a"] <- FALSE
  func <- build_allowed_function(mat)

  # 3-way: adding "c" to a term with "a" and "b"
  result <- func(degree = 3, pred = 3, parents = c(TRUE, TRUE, FALSE),
                 namesx = c("a", "b", "c"), first = FALSE)
  expect_false(result)
})

test_that("build_allowed_function rejects invalid matrix", {
  expect_error(build_allowed_function(matrix(1:4, 2, 2)), "logical matrix")
  expect_error(build_allowed_function(matrix(TRUE, 2, 3)), "square")
})
