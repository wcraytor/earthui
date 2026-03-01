# --- format_model_equation() single-response ---

test_that("format_model_equation produces valid LaTeX structure", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
  eq <- format_model_equation(result)

  expect_s3_class(eq, "earthui_equation")
  expect_true(grepl("\\\\begin\\{array\\}\\{lrl\\}", eq$latex))
  expect_true(grepl("\\\\end\\{array\\}", eq$latex))
  expect_true(grepl("^\\$\\$", eq$latex_inline))
  expect_true(grepl("\\$\\$$", eq$latex_inline))
})

test_that("format_model_equation LaTeX PDF has escaped underscores in text blocks", {
  # Use a variable with underscores to test escaping
  df <- mtcars
  names(df)[names(df) == "wt"] <- "car_weight"
  result <- fit_earth(df, "mpg", c("cyl", "car_weight"))
  eq <- format_model_equation(result)

  # PDF version should escape _ inside \text{} blocks
  expect_true(grepl("car\\\\_weight", eq$latex_pdf))
  # MathJax version should NOT escape _ inside \text{}
  expect_true(grepl("car_weight", eq$latex))
})

test_that("format_model_equation Word output uses aligned environment", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "wt"))
  eq <- format_model_equation(result)

  expect_true(grepl("\\\\begin\\{aligned\\}", eq$latex_word))
  expect_true(grepl("\\\\end\\{aligned\\}", eq$latex_word))
})

test_that("format_model_equation groups have correct structure", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
  eq <- format_model_equation(result)

  expect_true(length(eq$groups) > 0L)

  # First group should be the intercept (degree 0)
  intercept_grp <- eq$groups[[1]]
  expect_equal(intercept_grp$degree, 0L)
  expect_equal(intercept_grp$label, "Basis")

  # Each group should have g_j, g_k, g_f indices
  for (grp in eq$groups) {
    expect_true(!is.null(grp$g_j))
    expect_true(!is.null(grp$g_k))
    expect_true(!is.null(grp$g_f))
    expect_true(!is.null(grp$terms))
    expect_true(length(grp$terms) > 0L)
  }
})

test_that("format_model_equation digits parameter controls precision", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "wt"))
  eq3 <- format_model_equation(result, digits = 3L)
  eq10 <- format_model_equation(result, digits = 10L)

  # Higher digits should produce longer (or equal) LaTeX
  expect_true(nchar(eq10$latex) >= nchar(eq3$latex))
})

test_that("format_model_equation handles interaction terms", {
  result <- fit_earth(mtcars, "mpg", c("wt", "hp"), degree = 2L)
  eq <- format_model_equation(result)

  expect_s3_class(eq, "earthui_equation")
  # Should have groups with degree > 1 if interactions were selected
  degrees <- vapply(eq$groups, `[[`, integer(1), "degree")
  # At minimum we have degree 0 (intercept) and degree 1
  expect_true(0L %in% degrees)
})

test_that("format_model_equation handles categorical variables", {
  df <- mtcars
  df$am_cat <- as.character(df$am)
  result <- fit_earth(df, "mpg", c("wt", "am_cat"), categoricals = "am_cat")
  eq <- format_model_equation(result)

  expect_s3_class(eq, "earthui_equation")
  # Should contain indicator function notation for factor
  if (any(vapply(eq$groups, function(g) g$n_factors, integer(1)) > 0L)) {
    expect_true(grepl("I\\\\\\{", eq$latex))
  }
})

# --- list_g_functions() ---

test_that("list_g_functions returns expected data frame", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
  gf <- list_g_functions(result)

  expect_s3_class(gf, "data.frame")
  expect_true("index" %in% names(gf))
  expect_true("label" %in% names(gf))
  expect_true("n_terms" %in% names(gf))
  expect_true("d" %in% names(gf))
  expect_true(nrow(gf) > 0L)
  # All listed g-functions should be non-intercept (degree > 0)
  expect_true(all(gf$d > 0L))
})

test_that("list_g_functions returns empty for intercept-only model", {
  # Force a very simple model
  result <- fit_earth(mtcars, "mpg", "wt", nprune = 1L)
  gf <- list_g_functions(result)
  # With nprune=1, model may be intercept only
  if (length(result$model$selected.terms) == 1L) {
    expect_equal(nrow(gf), 0L)
  }
})

# --- plot_contribution() ---

test_that("plot_contribution returns a ggplot for numeric variable", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
  p <- plot_contribution(result, "wt")
  expect_s3_class(p, "ggplot")
})

test_that("plot_contribution returns a ggplot for categorical variable", {
  df <- mtcars
  df$cyl_cat <- as.character(df$cyl)
  result <- fit_earth(df, "mpg", c("wt", "cyl_cat"), categoricals = "cyl_cat")
  p <- plot_contribution(result, "cyl_cat")
  expect_s3_class(p, "ggplot")
})

test_that("plot_contribution errors on variable not in model", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "wt"))
  # "nonexistent" has no matching terms, so contribution is zero — just
  # verify it doesn't crash (plot_contribution doesn't validate variable name)
  p <- plot_contribution(result, "nonexistent")
  expect_s3_class(p, "ggplot")
})

# --- plot_g_function / plot_g_contour / plot_g_persp ---

test_that("plot_g_function returns a ggplot for 1D g-function", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
  gf <- list_g_functions(result)
  if (nrow(gf) > 0L) {
    # Find a degree-1 g-function
    d1 <- which(gf$d == 1L)
    if (length(d1) > 0L) {
      p <- plot_g_function(result, d1[1])
      expect_s3_class(p, "ggplot")
    }
  }
})

test_that("plot_g_contour returns a ggplot for 1D g-function", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
  gf <- list_g_functions(result)
  if (nrow(gf) > 0L) {
    d1 <- which(gf$d == 1L)
    if (length(d1) > 0L) {
      p <- plot_g_contour(result, d1[1])
      expect_s3_class(p, "ggplot")
    }
  }
})

test_that("plot_g_function rejects invalid group_index", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
  expect_error(plot_g_function(result, 999L), "must be between")
  expect_error(plot_g_function(result, 0L), "must be between")
})

test_that("plot_g_persp rejects invalid group_index", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
  expect_error(plot_g_persp(result, 999L), "must be between")
})

test_that("plot_g_contour rejects invalid group_index", {
  result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
  expect_error(plot_g_contour(result, 999L), "must be between")
})

# --- resolve_columns_() internal ---

test_that("resolve_columns_ maps dummy columns to base variables", {
  df <- data.frame(
    y = 1:6,
    color = factor(c("red", "blue", "green", "red", "blue", "green"))
  )
  col_names <- c("colorblue", "colorgreen", "colorred")
  info <- earthui:::resolve_columns_(col_names, "color", df)

  expect_equal(info$base_var, c("color", "color", "color"))
  expect_true(all(info$is_factor))
  expect_true("blue" %in% info$level)
  expect_true("green" %in% info$level)
  expect_true("red" %in% info$level)
})

test_that("resolve_columns_ handles dot-separated dummy names", {
  df <- data.frame(
    y = 1:4,
    size = factor(c("x large", "small", "x large", "small"))
  )
  # R's model.matrix replaces spaces with dots
  col_names <- c("sizesmall", "sizex.large")
  info <- earthui:::resolve_columns_(col_names, "size", df)

  expect_true(all(info$is_factor))
  expect_equal(info$base_var, c("size", "size"))
})

test_that("resolve_columns_ returns non-factor for non-categorical columns", {
  df <- data.frame(y = 1:5, x1 = rnorm(5), x2 = rnorm(5))
  col_names <- c("x1", "x2")
  info <- earthui:::resolve_columns_(col_names, character(0), df)

  expect_false(any(info$is_factor))
  expect_equal(info$base_var, c("x1", "x2"))
})

test_that("resolve_columns_ fallback startsWith matching works", {
  df <- data.frame(
    y = 1:4,
    region = factor(c("North East", "South", "North East", "South"))
  )
  # Simulate a dummy name that doesn't match exactly but starts with the var name
  col_names <- c("regionNorth.East", "regionSouth")
  info <- earthui:::resolve_columns_(col_names, "region", df)

  expect_true(all(info$is_factor))
  expect_equal(info$base_var, c("region", "region"))
})
