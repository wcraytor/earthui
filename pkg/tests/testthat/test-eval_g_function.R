# Tests for the internal function eval_g_function_()

# Helper: build a minimal mock model with a coefficient matrix
mock_model <- function(coefs, ncol = 1L) {
  m <- list()
  if (ncol == 1L) {
    m$coefficients <- matrix(coefs, ncol = 1L)
  } else {
    m$coefficients <- matrix(coefs, ncol = ncol)
  }
  m
}

make_group <- function(terms) list(terms = terms)

make_term <- function(index, components) {
  list(index = index, components = components)
}

make_comp <- function(base_var, dir, cut = 0, is_factor = FALSE, level = NULL) {
  list(base_var = base_var, is_factor = is_factor, level = level,
       dir = dir, cut = cut)
}

# ---- 1. dir=1 positive hinge: max(0, x - cut) ----------------------------
test_that("dir=1 computes positive hinge max(0, x - cut)", {
  model <- mock_model(c(10, 3))
  group <- make_group(list(
    make_term(2, list(make_comp("x1", dir = 1, cut = 5)))
  ))
  newdata <- data.frame(x1 = c(3, 5, 8, 10))
  result <- earthUI:::eval_g_function_(model, group, newdata)
  expect_equal(result, c(0, 0, 9, 15))
})

# ---- 2. dir=-1 negative hinge: max(0, cut - x) ---------------------------
test_that("dir=-1 computes negative hinge max(0, cut - x)", {
  model <- mock_model(c(0, 2))
  group <- make_group(list(
    make_term(2, list(make_comp("x1", dir = -1, cut = 6)))
  ))
  newdata <- data.frame(x1 = c(2, 6, 10))
  result <- earthUI:::eval_g_function_(model, group, newdata)
  expect_equal(result, c(8, 0, 0))
})

# ---- 3. dir=2 linear term: coef * x --------------------------------------
test_that("dir=2 computes linear term (coef * x)", {
  model <- mock_model(c(0, 4))
  group <- make_group(list(
    make_term(2, list(make_comp("x1", dir = 2, cut = 0)))
  ))
  newdata <- data.frame(x1 = c(-3, 0, 5))
  result <- earthUI:::eval_g_function_(model, group, newdata)
  expect_equal(result, c(-12, 0, 20))
})

# ---- 4. Factor term: is_factor=TRUE, level matching indicator -------------
test_that("factor term uses level matching indicator (0/1)", {
  model <- mock_model(c(0, 5))
  group <- make_group(list(
    make_term(2, list(make_comp("color", dir = 1, cut = 0,
                                is_factor = TRUE, level = "red")))
  ))
  newdata <- data.frame(color = c("red", "blue", "red", "green"),
                        stringsAsFactors = FALSE)
  result <- earthUI:::eval_g_function_(model, group, newdata)
  expect_equal(result, c(5, 0, 5, 0))
})

test_that("factor term works with R factor columns", {
  model <- mock_model(c(0, -2))
  group <- make_group(list(
    make_term(2, list(make_comp("size", dir = 1, cut = 0,
                                is_factor = TRUE, level = "large")))
  ))
  newdata <- data.frame(size = factor(c("small", "large", "medium")))
  result <- earthUI:::eval_g_function_(model, group, newdata)
  expect_equal(result, c(0, -2, 0))
})

# ---- 5. Missing column returns 0 -----------------------------------------
test_that("missing numeric column returns all zeros", {
  model <- mock_model(c(0, 7))
  group <- make_group(list(
    make_term(2, list(make_comp("nonexistent", dir = 1, cut = 3)))
  ))
  newdata <- data.frame(x1 = c(1, 2, 3))
  result <- earthUI:::eval_g_function_(model, group, newdata)
  expect_equal(result, c(0, 0, 0))
})

test_that("missing factor column returns all zeros", {
  model <- mock_model(c(0, 7))
  group <- make_group(list(
    make_term(2, list(make_comp("nonexistent", dir = 1, cut = 0,
                                is_factor = TRUE, level = "a")))
  ))
  newdata <- data.frame(x1 = c(1, 2))
  result <- earthUI:::eval_g_function_(model, group, newdata)
  expect_equal(result, c(0, 0))
})

# ---- 6. Multi-term group: sum of multiple terms --------------------------
test_that("multi-term group sums contributions from all terms", {
  model <- mock_model(c(100, 3, -2))
  group <- make_group(list(
    make_term(2, list(make_comp("x1", dir = 1, cut = 5))),
    make_term(3, list(make_comp("x1", dir = -1, cut = 5)))
  ))
  newdata <- data.frame(x1 = c(2, 5, 8))
  result <- earthUI:::eval_g_function_(model, group, newdata)
  expect_equal(result, c(-6, 0, 9))
})

test_that("multi-term group with different hinge cuts sums correctly", {
  model <- mock_model(c(0, 1, 2))
  group <- make_group(list(
    make_term(2, list(make_comp("x1", dir = 1, cut = 3))),
    make_term(3, list(make_comp("x1", dir = 1, cut = 7)))
  ))
  newdata <- data.frame(x1 = c(1, 5, 10))
  result <- earthUI:::eval_g_function_(model, group, newdata)
  expect_equal(result, c(0, 2, 13))
})

# ---- 7. Interaction terms: product of two hinge components ----------------
test_that("interaction term computes product of two hinge components", {
  model <- mock_model(c(0, 2))
  group <- make_group(list(
    make_term(2, list(
      make_comp("x1", dir = 1, cut = 3),
      make_comp("x2", dir = 1, cut = 10)
    ))
  ))
  newdata <- data.frame(x1 = c(1, 5, 5, 10), x2 = c(15, 8, 15, 20))
  result <- earthUI:::eval_g_function_(model, group, newdata)
  expect_equal(result, c(0, 0, 20, 140))
})

test_that("interaction with mixed dir values computes correctly", {
  model <- mock_model(c(0, 1.5))
  group <- make_group(list(
    make_term(2, list(
      make_comp("x1", dir = -1, cut = 4),
      make_comp("x2", dir = 1, cut = 2)
    ))
  ))
  newdata <- data.frame(x1 = c(1, 6, 2), x2 = c(5, 5, 1))
  result <- earthUI:::eval_g_function_(model, group, newdata)
  expect_equal(result, c(13.5, 0, 0))
})

test_that("interaction with factor and numeric hinge", {
  model <- mock_model(c(0, 10))
  group <- make_group(list(
    make_term(2, list(
      make_comp("color", dir = 1, cut = 0, is_factor = TRUE, level = "blue"),
      make_comp("x1", dir = 1, cut = 2)
    ))
  ))
  newdata <- data.frame(color = c("blue", "red", "blue"),
                        x1 = c(5, 5, 1), stringsAsFactors = FALSE)
  result <- earthUI:::eval_g_function_(model, group, newdata)
  expect_equal(result, c(30, 0, 0))
})

test_that("interaction with missing column in second component returns 0", {
  model <- mock_model(c(0, 5))
  group <- make_group(list(
    make_term(2, list(
      make_comp("x1", dir = 1, cut = 1),
      make_comp("missing_var", dir = 1, cut = 0)
    ))
  ))
  newdata <- data.frame(x1 = c(3, 5))
  result <- earthUI:::eval_g_function_(model, group, newdata)
  expect_equal(result, c(0, 0))
})

# ---- 8. response_idx for multi-target models ------------------------------
test_that("response_idx selects correct coefficient column", {
  coef_matrix <- c(0, 3, -1, 0, 7, -4)
  model <- mock_model(coef_matrix, ncol = 2L)
  group <- make_group(list(
    make_term(2, list(make_comp("x1", dir = 1, cut = 5)))
  ))
  newdata <- data.frame(x1 = c(8, 3))

  r1 <- earthUI:::eval_g_function_(model, group, newdata, response_idx = 1L)
  expect_equal(r1, c(9, 0))

  r2 <- earthUI:::eval_g_function_(model, group, newdata, response_idx = 2L)
  expect_equal(r2, c(21, 0))
})

test_that("response_idx=NULL defaults to first column for multi-target", {
  coef_matrix <- c(0, 10, 0, 99)
  model <- mock_model(coef_matrix, ncol = 2L)
  group <- make_group(list(
    make_term(2, list(make_comp("x1", dir = 2, cut = 0)))
  ))
  newdata <- data.frame(x1 = c(3))
  result <- earthUI:::eval_g_function_(model, group, newdata, response_idx = NULL)
  expect_equal(result, 30)
})

# ---- Edge cases -----------------------------------------------------------
test_that("empty group terms returns all zeros", {
  model <- mock_model(c(5))
  group <- make_group(list())
  newdata <- data.frame(x1 = c(1, 2, 3))
  result <- earthUI:::eval_g_function_(model, group, newdata)
  expect_equal(result, c(0, 0, 0))
})

test_that("single row newdata works correctly", {
  model <- mock_model(c(0, 2.5))
  group <- make_group(list(
    make_term(2, list(make_comp("x1", dir = 1, cut = 3)))
  ))
  newdata <- data.frame(x1 = 10)
  result <- earthUI:::eval_g_function_(model, group, newdata)
  expect_equal(result, 17.5)
})
