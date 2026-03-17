# Tests for the plotly surface transposition fix in plot_g_3d_().
#
# The bug: z_mat was passed to plotly::add_surface() without transposing.
# Plotly convention: z[i,j] corresponds to (x[j], y[i]).
# Our matrix fill: z_mat[i,j] corresponds to (x1_seq[i], x2_seq[j]).
# Therefore the fix is to pass t(z_mat) to add_surface().

# --- Fit a degree-2 earth model to get interaction terms ---
result_d2 <- fit_earth(
  mtcars, "mpg", c("wt", "hp", "disp"),
  degree = 2L
)

eq_d2 <- format_model_equation(result_d2, response_idx = 1L)
all_groups <- eq_d2$groups
non_intercept <- Filter(function(g) g$degree > 0L, all_groups)
interaction_groups <- Filter(function(g) g$degree >= 2L, non_intercept)

# Find the group index (1-based among non-intercept groups) for the first
# interaction group -- needed for plot_g_function().
interaction_idx <- which(vapply(non_intercept, function(g) g$degree >= 2L,
                                logical(1)))[1]

test_that("degree-2 model has at least one interaction group", {
  skip_if(length(interaction_groups) == 0L,
          "No interaction terms in the degree-2 fit; cannot test 3D surface")
  expect_true(length(interaction_groups) >= 1L)
})

# Only run the remaining tests if we actually got an interaction group.
skip_if(length(interaction_groups) == 0L,
        "No interaction terms; skipping transposition tests")

grp <- interaction_groups[[1]]
var1 <- grp$base_vars[1]
var2 <- grp$base_vars[2]
model <- result_d2$model
data <- result_d2$data

# --- Test 1: data point contributions match eval_g_function_ ---
test_that("eval_g_function_ matches bx * coefs for data points", {
  # Evaluate g-function at each observation's actual coordinates
  g_vals <- earthUI:::eval_g_function_(model, grp, data, response_idx = 1L)

  # Compute the same contribution from bx and coefficients directly
  term_indices <- vapply(grp$terms, function(t) t$index, integer(1))
  coefs <- as.numeric(model$coefficients)
  contrib <- rowSums(sweep(model$bx[, term_indices, drop = FALSE], 2,
                           coefs[term_indices], "*"))

  expect_equal(g_vals, contrib, tolerance = 1e-10)
})

# --- Test 2: z_mat fill convention ---
# Verify that z_mat[i,j] = eval_g_function_ at (x1_seq[i], x2_seq[j])
test_that("z_mat[i,j] corresponds to (x1_seq[i], x2_seq[j])", {
  n_grid <- 50L
  x1_seq <- seq(min(data[[var1]], na.rm = TRUE),
                max(data[[var1]], na.rm = TRUE), length.out = n_grid)
  x2_seq <- seq(min(data[[var2]], na.rm = TRUE),
                max(data[[var2]], na.rm = TRUE), length.out = n_grid)
  grid <- expand.grid(x1 = x1_seq, x2 = x2_seq)

  eval_data <- data[rep(1L, nrow(grid)), , drop = FALSE]
  eval_data[[var1]] <- grid$x1
  eval_data[[var2]] <- grid$x2
  z_vals <- earthUI:::eval_g_function_(model, grp, eval_data, response_idx = 1L)
  z_mat <- matrix(z_vals, nrow = n_grid, ncol = n_grid)

  # Spot-check: z_mat[i,j] should equal eval at (x1_seq[i], x2_seq[j])
  check_pairs <- list(c(1, 1), c(1, 50), c(50, 1), c(50, 50),
                       c(25, 10), c(10, 25), c(37, 42))
  for (pair in check_pairs) {
    i <- pair[1]
    j <- pair[2]
    # Build a single-row data frame at (x1_seq[i], x2_seq[j])
    point_data <- data[1L, , drop = FALSE]
    point_data[[var1]] <- x1_seq[i]
    point_data[[var2]] <- x2_seq[j]
    expected <- earthUI:::eval_g_function_(model, grp, point_data,
                                           response_idx = 1L)
    expect_equal(z_mat[i, j], expected,
                 tolerance = 1e-12,
                 info = sprintf("z_mat[%d,%d] at (%s=%g, %s=%g)",
                                i, j, var1, x1_seq[i], var2, x2_seq[j]))
  }
})

# --- Test 3: plotly object receives t(z_mat) ---
test_that("plotly surface z-data equals t(z_mat)", {
  skip_if_not_installed("plotly")

  # Build z_mat the same way plot_g_3d_ does
  n_grid <- 50L
  x1_seq <- seq(min(data[[var1]], na.rm = TRUE),
                max(data[[var1]], na.rm = TRUE), length.out = n_grid)
  x2_seq <- seq(min(data[[var2]], na.rm = TRUE),
                max(data[[var2]], na.rm = TRUE), length.out = n_grid)
  grid <- expand.grid(x1 = x1_seq, x2 = x2_seq)

  eval_data <- data[rep(1L, nrow(grid)), , drop = FALSE]
  eval_data[[var1]] <- grid$x1
  eval_data[[var2]] <- grid$x2
  z_vals <- earthUI:::eval_g_function_(model, grp, eval_data, response_idx = 1L)
  z_mat <- matrix(z_vals, nrow = n_grid, ncol = n_grid)

  # Call the public plotting function to get the plotly object
  fig <- plot_g_function(result_d2, group_index = interaction_idx,
                         response_idx = 1L)
  expect_s3_class(fig, "plotly")

  # Extract the surface trace z-data from the plotly object.
  built <- plotly::plotly_build(fig)
  traces <- built$x$data

  # Find the surface trace
  surface_traces <- Filter(function(tr) identical(tr$type, "surface"), traces)
  expect_true(length(surface_traces) >= 1L,
              info = "Expected at least one surface trace in the plotly object")

  surface_z <- surface_traces[[1]]$z

  # surface_z may be a matrix or a list of rows; normalize to matrix
  if (is.list(surface_z) && !is.matrix(surface_z)) {
    z_from_plotly <- do.call(rbind, surface_z)
  } else {
    z_from_plotly <- as.matrix(surface_z)
  }

  # The fix: plotly should receive t(z_mat)
  expected_z <- t(z_mat)

  # Strip plotly attributes before comparing
  expect_equal(unclass(z_from_plotly), unclass(expected_z), tolerance = 1e-10,
               ignore_attr = TRUE,
               info = "Plotly surface z should be t(z_mat), not z_mat")

  expect_equal(nrow(z_from_plotly), n_grid)
  expect_equal(ncol(z_from_plotly), n_grid)

  # Verify it is NOT the untransposed matrix (unless symmetric, which is
  # extremely unlikely for real data)
  if (!isTRUE(all.equal(z_mat, t(z_mat), tolerance = 1e-10))) {
    expect_false(isTRUE(all.equal(z_from_plotly, z_mat, tolerance = 1e-10)),
                 info = "Plotly z should NOT equal the untransposed z_mat")
  }
})
