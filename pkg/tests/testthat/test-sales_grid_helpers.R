# test-sales_grid_helpers.R — Tests for sales_grid.R helper functions
#
# We source the sales_grid.R file directly and test the helper functions
# without generating full Excel output (which requires openxlsx).

# Source the sales grid helper functions
sg_path <- system.file("app", "sales_grid.R", package = "earthUI")
if (nzchar(sg_path)) {
  # Only define helpers if not already sourced; avoid library() side effects
  sg_env <- new.env(parent = globalenv())
  # Pre-load openxlsx and readxl to prevent library() calls from erroring
  if (requireNamespace("openxlsx", quietly = TRUE) &&
      requireNamespace("readxl", quietly = TRUE)) {
    local({
      # Suppress library attach messages
      suppressPackageStartupMessages({
        source(sg_path, local = sg_env)
      })
    })
  }
}

# ---- haversine_miles ----

test_that("haversine_miles computes correct distance", {
  skip_if(!exists("sg_env") || !exists("haversine_miles", envir = sg_env))
  hm <- sg_env$haversine_miles

  # NYC to LA: ~2451 miles
  d <- hm(40.7128, -74.0060, 34.0522, -118.2437)
  expect_true(d > 2400 && d < 2500)

  # Same point = 0
  expect_equal(hm(38.5, -121.4, 38.5, -121.4), 0)
})

test_that("haversine_miles returns NA for missing coords", {
  skip_if(!exists("sg_env") || !exists("haversine_miles", envir = sg_env))
  hm <- sg_env$haversine_miles
  expect_true(is.na(hm(NA, -121, 38, -121)))
  expect_true(is.na(hm(38, NA, 38, -121)))
  expect_true(is.na(hm(38, -121, NA, -121)))
  expect_true(is.na(hm(38, -121, 38, NA)))
})

# ---- col_val ----

test_that("col_val retrieves values safely", {
  skip_if(!exists("sg_env") || !exists("col_val", envir = sg_env))
  cv <- sg_env$col_val

  df <- data.frame(a = c(10, 20, NA), b = c("x", "y", "z"),
                   stringsAsFactors = FALSE)
  expect_equal(cv(df, 1, "a"), 10)
  expect_equal(cv(df, 2, "b"), "y")
  expect_equal(cv(df, 3, "a"), "")           # NA -> default ""
  expect_equal(cv(df, 1, "missing_col"), "") # missing col -> default
  expect_equal(cv(df, 1, "missing_col", default = 0), 0)
})

# ---- col_num ----

test_that("col_num retrieves numeric values with rounding", {
  skip_if(!exists("sg_env") || !exists("col_num", envir = sg_env))
  cn <- sg_env$col_num

  df <- data.frame(price = c(123456.789, NA, 200000))
  expect_equal(cn(df, 1, "price", digits = 2), 123456.79)
  expect_equal(cn(df, 1, "price", digits = 0), 123457)
  expect_equal(cn(df, 2, "price"), 0)         # NA -> default 0
  expect_equal(cn(df, 1, "nonexistent"), 0)   # missing col -> default
})

# ---- compute_dom ----

test_that("compute_dom calculates days on market", {
  skip_if(!exists("sg_env") || !exists("compute_dom", envir = sg_env))
  cd <- sg_env$compute_dom

  df <- data.frame(
    contract_date = as.Date(c("2025-06-15", "2025-03-01")),
    listing_date  = as.Date(c("2025-05-15", "2025-01-01"))
  )
  expect_equal(cd(df, 1), 31L)
  expect_equal(cd(df, 2), 59L)
})

test_that("compute_dom returns NA when dates missing", {
  skip_if(!exists("sg_env") || !exists("compute_dom", envir = sg_env))
  cd <- sg_env$compute_dom

  df <- data.frame(contract_date = as.Date(NA), listing_date = as.Date(NA))
  expect_true(is.na(cd(df, 1)))

  # Missing columns
  df2 <- data.frame(x = 1)
  expect_true(is.na(cd(df2, 1)))
})

# ---- detect_model_vars ----

test_that("detect_model_vars finds contribution/adjustment pairs", {
  skip_if(!exists("sg_env") || !exists("detect_model_vars", envir = sg_env))
  dmv <- sg_env$detect_model_vars

  df <- data.frame(
    sale_age_contribution = c(100, 200),
    sale_age_adjustment   = c(0, -100),
    living_sqft_contribution = c(300, 250),
    living_sqft_adjustment   = c(0, 50),
    residual = c(10, -10)
  )
  result <- dmv(df)
  expect_equal(result$labels, c("sale_age", "living_sqft"))
  expect_equal(result$contrib, c("sale_age_contribution", "living_sqft_contribution"))
  expect_equal(result$adjustment, c("sale_age_adjustment", "living_sqft_adjustment"))
})

test_that("detect_model_vars filters out rent_ prefix", {
  skip_if(!exists("sg_env") || !exists("detect_model_vars", envir = sg_env))
  dmv <- sg_env$detect_model_vars

  df <- data.frame(
    age_contribution      = c(100, 200),
    age_adjustment        = c(0, -100),
    rent_age_contribution = c(50, 60),
    rent_age_adjustment   = c(0, -10)
  )
  result <- dmv(df)
  expect_equal(result$labels, "age")
  expect_length(result$labels, 1)
})

# ---- format_label ----

test_that("format_label abbreviates known labels", {
  skip_if(!exists("sg_env") || !exists("format_label", envir = sg_env))
  fl <- sg_env$format_label

  expect_equal(fl("living_sqft"), "Living SF")
  expect_equal(fl("baths_total"), "Baths")
  expect_equal(fl("garage_spaces"), "Garage")
  expect_equal(fl("age"), "Age")
})

test_that("format_label replaces underscores for unknown labels", {
  skip_if(!exists("sg_env") || !exists("format_label", envir = sg_env))
  fl <- sg_env$format_label

  expect_equal(fl("some_custom_var"), "some custom var")
})

test_that("format_label truncates long labels", {
  skip_if(!exists("sg_env") || !exists("format_label", envir = sg_env))
  fl <- sg_env$format_label

  long <- paste0(rep("x", 40), collapse = "")
  result <- fl(long)
  expect_true(nchar(result) <= 28)
  expect_true(grepl("\\.\\.\\.$", result))
})

# ---- col_letter ----

test_that("col_letter converts numbers to Excel column letters", {
  skip_if(!exists("sg_env") || !exists("col_letter", envir = sg_env))
  cl <- sg_env$col_letter

  expect_equal(cl(1), "A")
  expect_equal(cl(5), "E")
  expect_equal(cl(26), "Z")
  expect_equal(cl(27), "AA")
  expect_equal(cl(28), "AB")
})

# ---- sp_col ----

test_that("sp_col returns column name or NULL", {
  skip_if(!exists("sg_env") || !exists("sp_col", envir = sg_env))
  sc <- sg_env$sp_col

  specials <- list(latitude = "lat_col", longitude = "lon_col")
  expect_equal(sc(specials, "latitude"), "lat_col")
  expect_null(sc(specials, "area"))
})

# ---- sum_contribs ----

test_that("sum_contribs sums contribution columns for given vars", {
  skip_if(!exists("sg_env") || !exists("sum_contribs", envir = sg_env))
  sc <- sg_env$sum_contribs

  df <- data.frame(
    lat_contribution = c(100, 200),
    lon_contribution = c(50, 75),
    age_contribution = c(-20, -30)
  )
  expect_equal(sc(df, 1, c("lat", "lon")), 150)
  expect_equal(sc(df, 2, c("lat", "lon", "age")), 245)
  expect_equal(sc(df, 1, c("nonexistent")), 0)
})
