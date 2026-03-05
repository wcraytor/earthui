# Tests for HTML equation formatters in format_results.R

# -- format_component_html_ ----------------------------------------------------

test_that("format_component_html_ handles dir=1 (positive hinge)", {
  comp <- list(base_var = "sq_ft", is_factor = FALSE, dir = 1, cut = 1500)
  result <- earthUI:::format_component_html_(comp)
  expect_equal(result, "max(0, sq_ft - 1500)")
})

test_that("format_component_html_ handles dir=-1 (negative hinge)", {
  comp <- list(base_var = "sq_ft", is_factor = FALSE, dir = -1, cut = 1500)
  result <- earthUI:::format_component_html_(comp)
  expect_equal(result, "max(0, 1500 - sq_ft)")
})

test_that("format_component_html_ handles dir=2 (linear predictor)", {
  comp <- list(base_var = "wt", is_factor = FALSE, dir = 2)
  result <- earthUI:::format_component_html_(comp)
  expect_equal(result, "wt")
})

test_that("format_component_html_ handles factor indicator", {
  comp <- list(base_var = "condition", is_factor = TRUE, level = "Good")
  result <- earthUI:::format_component_html_(comp)
  expect_equal(result, "I{condition = Good}")
})

# -- format_term_html_ ---------------------------------------------------------

test_that("format_term_html_ formats intercept (degree 0)", {
  term <- list(coefficient = 42.5, degree = 0L, components = list())
  result <- earthUI:::format_term_html_(term, is_first = TRUE, digits = 4L)
  expect_equal(result, "42.5")
})

test_that("format_term_html_ formats first term (no sign prefix)", {
  comp <- list(base_var = "wt", is_factor = FALSE, dir = 1, cut = 3.0)
  term <- list(coefficient = -5.2, degree = 1L, components = list(comp))
  result <- earthUI:::format_term_html_(term, is_first = TRUE, digits = 4L)
  expect_match(result, "^-5\\.2 \\* max\\(0, wt - 3\\)")
})

test_that("format_term_html_ formats non-first positive term with + sign", {
  comp <- list(base_var = "hp", is_factor = FALSE, dir = -1, cut = 200)
  term <- list(coefficient = 3.7, degree = 1L, components = list(comp))
  result <- earthUI:::format_term_html_(term, is_first = FALSE, digits = 4L)
  expect_match(result, "^\\+ 3\\.7 \\* max\\(0, 200 - hp\\)")
})

test_that("format_term_html_ formats non-first negative term with - sign", {
  comp <- list(base_var = "hp", is_factor = FALSE, dir = 1, cut = 100)
  term <- list(coefficient = -2.5, degree = 1L, components = list(comp))
  result <- earthUI:::format_term_html_(term, is_first = FALSE, digits = 4L)
  expect_match(result, "^- 2\\.5 \\* max\\(0, hp - 100\\)")
})

test_that("format_term_html_ formats interaction (degree 2)", {
  comp1 <- list(base_var = "wt", is_factor = FALSE, dir = 1, cut = 3.0)
  comp2 <- list(base_var = "hp", is_factor = FALSE, dir = -1, cut = 200)
  term <- list(coefficient = 1.5, degree = 2L, components = list(comp1, comp2))
  result <- earthUI:::format_term_html_(term, is_first = FALSE, digits = 4L)
  expect_match(result, "max\\(0, wt - 3\\) \\* max\\(0, 200 - hp\\)")
})

test_that("format_term_html_ escapes HTML characters in output", {
  # Variable name with special chars would need escaping
  comp <- list(base_var = "a<b", is_factor = FALSE, dir = 1, cut = 5)
  term <- list(coefficient = 1.0, degree = 1L, components = list(comp))
  result <- earthUI:::format_term_html_(term, is_first = TRUE, digits = 4L)
  expect_match(result, "&lt;")  # < should be escaped
})

# -- Integration: HTML formatters work on real model terms ---------------------

test_that("format_component_html_ round-trips through real model terms", {
  result <- fit_earth(mtcars, "mpg", c("wt", "hp"))
  eq <- format_model_equation(result)

  # Each group has term_info with components; test them all format without error
  for (g in eq$groups) {
    for (ti in g$terms) {
      if (ti$degree > 0L) {
        for (comp in ti$components) {
          out <- earthUI:::format_component_html_(comp)
          expect_type(out, "character")
          expect_true(nchar(out) > 0L)
        }
      }
    }
  }
})
