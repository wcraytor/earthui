# --- Sales Grid helper functions ---
# Source the sales_grid.R script to get helper functions into scope
sales_grid_path <- system.file("app_glmnet", "sales_grid.R", package = "earthUI")

# Skip all tests if sales_grid.R not found (e.g., during devtools::test before install)
if (!nzchar(sales_grid_path)) {
  sales_grid_path <- file.path(
    testthat::test_path(), "..", "..", "inst", "app", "sales_grid.R")
}

# Source helpers only (suppress library calls by pre-loading namespace check)
if (file.exists(sales_grid_path)) {
  # Source in a local env to extract helpers
  sg_env <- new.env(parent = globalenv())
  tryCatch(
    source(sales_grid_path, local = sg_env),
    error = function(e) NULL  # skip if openxlsx/readxl not available
  )
}

skip_if_no_sg <- function() {
  if (!exists("sg_env") || !exists("col_val", envir = sg_env)) {
    testthat::skip("sales_grid.R helpers not available")
  }
}

# --- col_val ---
test_that("col_val returns value when column exists", {
  skip_if_no_sg()
  df <- data.frame(a = c(10, 20), b = c("x", "y"),
                    stringsAsFactors = FALSE)
  expect_equal(sg_env$col_val(df, 1, "a"), 10)
  expect_equal(sg_env$col_val(df, 2, "b"), "y")
})

test_that("col_val returns default for missing column", {
  skip_if_no_sg()
  df <- data.frame(a = 1:3)
  expect_equal(sg_env$col_val(df, 1, "nonexistent"), "")
  expect_equal(sg_env$col_val(df, 1, "nonexistent", default = "N/A"), "N/A")
})

test_that("col_val returns default for NA value", {
  skip_if_no_sg()
  df <- data.frame(a = c(NA_real_, 5))
  expect_equal(sg_env$col_val(df, 1, "a"), "")
  expect_equal(sg_env$col_val(df, 1, "a", default = 0), 0)
})

# --- col_num ---
test_that("col_num returns rounded numeric", {
  skip_if_no_sg()
  df <- data.frame(price = c(123456.789, 200000.5))
  expect_equal(sg_env$col_num(df, 1, "price", digits = 0), 123457)
  expect_equal(sg_env$col_num(df, 1, "price", digits = 2), 123456.79)
})

test_that("col_num returns default for missing column", {
  skip_if_no_sg()
  df <- data.frame(a = 1)
  expect_equal(sg_env$col_num(df, 1, "nonexistent"), 0)
  expect_equal(sg_env$col_num(df, 1, "nonexistent", default = -1), -1)
})

# --- col_letter ---
test_that("col_letter returns correct Excel column letters", {
  skip_if_no_sg()
  expect_equal(sg_env$col_letter(1), "A")
  expect_equal(sg_env$col_letter(26), "Z")
  expect_equal(sg_env$col_letter(27), "AA")
  expect_equal(sg_env$col_letter(28), "AB")
})

# --- haversine_miles ---
test_that("haversine_miles computes distance correctly", {
  skip_if_no_sg()
  # Same point = 0 miles
  expect_equal(sg_env$haversine_miles(37.7749, -122.4194, 37.7749, -122.4194), 0)
  # SF to LA ~ 347 miles (approximate)
  dist <- sg_env$haversine_miles(37.7749, -122.4194, 34.0522, -118.2437)
  expect_true(dist > 340 && dist < 360)
})

test_that("haversine_miles returns NA for NA inputs", {
  skip_if_no_sg()
  expect_true(is.na(sg_env$haversine_miles(NA, -122, 34, -118)))
  expect_true(is.na(sg_env$haversine_miles(37, NA, 34, -118)))
})

# --- format_label ---
test_that("format_label abbreviates known labels", {
  skip_if_no_sg()
  expect_equal(sg_env$format_label("living_sqft"), "Living SF")
  expect_equal(sg_env$format_label("lot_size"), "Lot Size")
  expect_equal(sg_env$format_label("sale_age"), "Sale Age")
  expect_equal(sg_env$format_label("beds_total"), "Beds")
})

test_that("format_label converts underscores to spaces for unknown labels", {
  skip_if_no_sg()
  expect_equal(sg_env$format_label("pool_count"), "pool count")
})

test_that("format_label truncates long labels", {
  skip_if_no_sg()
  long_label <- paste(rep("a", 40), collapse = "_")
  result <- sg_env$format_label(long_label)
  expect_true(nchar(result) <= 28)
})

# --- detect_model_vars ---
test_that("detect_model_vars finds contribution/adjustment columns", {
  skip_if_no_sg()
  df <- data.frame(
    sqft_contribution = 1:3,
    sqft_adjustment = 4:6,
    age_contribution = 7:9,
    age_adjustment = 10:12,
    other_col = 13:15
  )
  mv <- sg_env$detect_model_vars(df)
  expect_equal(mv$labels, c("sqft", "age"))
  expect_equal(mv$contrib, c("sqft_contribution", "age_contribution"))
  expect_equal(mv$adjustment, c("sqft_adjustment", "age_adjustment"))
})

test_that("detect_model_vars excludes rent_ prefixed", {
  skip_if_no_sg()
  df <- data.frame(
    sqft_contribution = 1:3,
    sqft_adjustment = 4:6,
    rent_sqft_contribution = 7:9,
    rent_sqft_adjustment = 10:12
  )
  mv <- sg_env$detect_model_vars(df)
  expect_equal(mv$labels, "sqft")
})

# --- sp_col ---
test_that("sp_col returns column name or NULL", {
  skip_if_no_sg()
  specials <- list(latitude = "lat_col", longitude = "lon_col")
  expect_equal(sg_env$sp_col(specials, "latitude"), "lat_col")
  expect_null(sg_env$sp_col(specials, "area"))
})

# --- sum_contribs ---
test_that("sum_contribs sums contribution columns", {
  skip_if_no_sg()
  df <- data.frame(
    sqft_contribution = c(100, 200, 300),
    age_contribution = c(-10, -20, -30)
  )
  expect_equal(sg_env$sum_contribs(df, 1, c("sqft", "age")), 90)
  expect_equal(sg_env$sum_contribs(df, 2, c("sqft", "age")), 180)
})

test_that("sum_contribs returns 0 for missing columns", {
  skip_if_no_sg()
  df <- data.frame(x = 1:3)
  expect_equal(sg_env$sum_contribs(df, 1, c("nonexistent")), 0)
})

# --- generate_sales_grid integration test ---
test_that("generate_sales_grid creates an Excel file", {
  skip_if_no_sg()
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  # Build a minimal RCA-like data frame
  df <- data.frame(
    street_address = c("123 Subject St", "456 Comp Ave", "789 Comp Blvd",
                        "101 Comp Dr"),
    city_name = rep("Testville", 4),
    postal_code = rep("12345", 4),
    parcel_number = paste0("APN-", 1:4),
    listing_id = paste0("MLS-", 1:4),
    sale_price = c(NA, 300000, 310000, 290000),
    sale_age = c(0, 30, 60, 90),
    basis = rep(250000, 4),
    sqft_contribution = c(30000, 28000, 35000, 25000),
    sqft_adjustment = c(0, 2000, -5000, 5000),
    age_contribution = c(-5000, -10000, -15000, -3000),
    age_adjustment = c(0, 5000, 10000, -2000),
    residual = c(5000, 2000, -3000, 1000),
    cqa = c(5.00, 6.50, 3.20, 7.10),
    subject_cqa = c(5.00, NA, NA, NA),
    net_adjustments = c(NA, 7000, 5000, 3000),
    gross_adjustments = c(NA, 7000, 15000, 7000),
    stringsAsFactors = FALSE
  )

  tmp_input <- tempfile(fileext = ".xlsx")
  tmp_output <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(df, tmp_input)

  result <- sg_env$generate_sales_grid(
    adjusted_file = tmp_input,
    comp_rows = c(2, 3, 4),
    output_file = tmp_output
  )

  expect_true(file.exists(tmp_output))
  expect_equal(result, tmp_output)

  # Verify workbook has expected sheet
  sheets <- openxlsx::getSheetNames(tmp_output)
  expect_equal(length(sheets), 1)  # 3 comps = 1 sheet
  expect_match(sheets[1], "Comps")

  unlink(c(tmp_input, tmp_output))
})

# ===================================================================
# Edge case tests for helper functions
# ===================================================================

# --- col_letter edge cases ---
test_that("col_letter handles large column numbers", {
  skip_if_no_sg()
  expect_equal(sg_env$col_letter(52), "AZ")
  expect_equal(sg_env$col_letter(53), "BA")
})

test_that("col_letter handles single-letter boundary", {
  skip_if_no_sg()
  expect_equal(sg_env$col_letter(1), "A")
  expect_equal(sg_env$col_letter(26), "Z")
  # Just past boundary

  expect_equal(sg_env$col_letter(27), "AA")
})

# --- haversine_miles edge cases ---
test_that("haversine_miles handles antipodal points", {
  skip_if_no_sg()
  # North pole to south pole ~ 12,436 miles (half Earth circumference)
  dist <- sg_env$haversine_miles(90, 0, -90, 0)
  expect_true(dist > 12400 && dist < 12500)
})

test_that("haversine_miles handles zero distance", {
  skip_if_no_sg()
  expect_equal(sg_env$haversine_miles(0, 0, 0, 0), 0)
})

test_that("haversine_miles returns NA when any coordinate is NA", {
  skip_if_no_sg()
  expect_true(is.na(sg_env$haversine_miles(37, -122, NA, -118)))
  expect_true(is.na(sg_env$haversine_miles(37, -122, 34, NA)))
  expect_true(is.na(sg_env$haversine_miles(NA, NA, NA, NA)))
})

# --- format_label edge cases ---
test_that("format_label handles interaction terms with colons", {
  skip_if_no_sg()
  result <- sg_env$format_label("sqft:age")
  # Should convert underscores/colons to something readable

  expect_true(nchar(result) > 0)
})

test_that("format_label handles all known abbreviations", {
  skip_if_no_sg()
  expect_equal(sg_env$format_label("baths_total"), "Baths")
  expect_equal(sg_env$format_label("garage_spaces"), "Garage")
  expect_equal(sg_env$format_label("fp_count"), "Fireplaces")
  expect_equal(sg_env$format_label("no_of_stories"), "Stories")
  expect_equal(sg_env$format_label("year_built"), "Year Built")
  expect_equal(sg_env$format_label("days_on_market"), "DOM")
  expect_equal(sg_env$format_label("contract_date"), "Contract Date")
  expect_equal(sg_env$format_label("area_id"), "Area")
  expect_equal(sg_env$format_label("area_text"), "Area")
  expect_equal(sg_env$format_label("latitude"), "Latitude")
  expect_equal(sg_env$format_label("longitude"), "Longitude")
  expect_equal(sg_env$format_label("age"), "Age")
})

test_that("format_label handles empty string", {
  skip_if_no_sg()
  result <- sg_env$format_label("")
  expect_true(is.character(result))
})

# --- detect_model_vars edge cases ---
test_that("detect_model_vars handles interaction contributions", {
  skip_if_no_sg()
  df <- data.frame(
    sqft_x_age_contribution = 1:3,
    sqft_x_age_adjustment = 4:6,
    beds_contribution = 7:9,
    beds_adjustment = 10:12
  )
  mv <- sg_env$detect_model_vars(df)
  expect_true("sqft_x_age" %in% mv$labels)
  expect_true("beds" %in% mv$labels)
})

test_that("detect_model_vars handles no model columns", {
  skip_if_no_sg()
  df <- data.frame(a = 1:3, b = 4:6)
  mv <- sg_env$detect_model_vars(df)
  expect_equal(length(mv$labels), 0)
  expect_equal(length(mv$contrib), 0)
  expect_equal(length(mv$adjustment), 0)
})

test_that("detect_model_vars handles contribution without matching adjustment", {
  skip_if_no_sg()
  df <- data.frame(
    sqft_contribution = 1:3,
    sqft_adjustment = 4:6,
    orphan_contribution = 7:9
    # no orphan_adjustment
  )
  mv <- sg_env$detect_model_vars(df)
  # orphan should be excluded because adjustment column is missing
  expect_false("orphan" %in% mv$labels)
  expect_true("sqft" %in% mv$labels)
})

# --- col_val edge cases ---
test_that("col_val handles empty string value", {
  skip_if_no_sg()
  df <- data.frame(a = c("", "hello"), stringsAsFactors = FALSE)
  # Empty string is not NA, should return it
  expect_equal(sg_env$col_val(df, 1, "a"), "")
})

test_that("col_val handles logical column", {
  skip_if_no_sg()
  df <- data.frame(flag = c(TRUE, FALSE))
  expect_equal(sg_env$col_val(df, 1, "flag"), TRUE)
  expect_equal(sg_env$col_val(df, 2, "flag"), FALSE)
})

# --- col_num edge cases ---
test_that("col_num handles character that looks numeric", {
  skip_if_no_sg()
  df <- data.frame(x = c("123.45", "678.90"), stringsAsFactors = FALSE)
  expect_equal(sg_env$col_num(df, 1, "x", digits = 1), 123.4)
})

test_that("col_num handles NA value", {
  skip_if_no_sg()
  df <- data.frame(x = c(NA_real_, 100))
  expect_equal(sg_env$col_num(df, 1, "x"), 0)
  expect_equal(sg_env$col_num(df, 1, "x", default = -999), -999)
})

test_that("col_num handles negative values", {
  skip_if_no_sg()
  df <- data.frame(x = c(-5000.678))
  expect_equal(sg_env$col_num(df, 1, "x", digits = 0), -5001)
  expect_equal(sg_env$col_num(df, 1, "x", digits = 2), -5000.68)
})

# --- sum_contribs edge cases ---
test_that("sum_contribs handles empty var_names", {
  skip_if_no_sg()
  df <- data.frame(sqft_contribution = c(100, 200))
  expect_equal(sg_env$sum_contribs(df, 1, character(0)), 0)
})

test_that("sum_contribs handles mix of existing and missing columns", {
  skip_if_no_sg()
  df <- data.frame(sqft_contribution = c(100, 200))
  # sqft exists, age does not
  expect_equal(sg_env$sum_contribs(df, 1, c("sqft", "age")), 100)
})

# --- sp_col edge cases ---
test_that("sp_col handles empty specials list", {
  skip_if_no_sg()
  expect_null(sg_env$sp_col(list(), "latitude"))
})

test_that("sp_col handles NULL specials", {
  skip_if_no_sg()
  expect_null(sg_env$sp_col(NULL, "latitude"))
})

# --- compute_dom ---
test_that("compute_dom returns days between listing and contract", {
  skip_if_no_sg()
  df <- data.frame(
    contract_date = as.character(as.Date("2025-06-15")),
    listing_date = as.character(as.Date("2025-05-01")),
    stringsAsFactors = FALSE
  )
  expect_equal(sg_env$compute_dom(df, 1), 45L)
})

test_that("compute_dom returns NA when dates are missing", {
  skip_if_no_sg()
  df <- data.frame(
    contract_date = NA_character_,
    listing_date = "2025-05-01",
    stringsAsFactors = FALSE
  )
  expect_true(is.na(sg_env$compute_dom(df, 1)))
})

test_that("compute_dom returns NA when columns are absent", {
  skip_if_no_sg()
  df <- data.frame(x = 1)
  expect_true(is.na(sg_env$compute_dom(df, 1)))
})

# ===================================================================
# Integration tests for generate_sales_grid
# ===================================================================

# Helper to build a minimal RCA dataframe
build_test_rca <- function(n_comps, include_location = FALSE,
                            include_site = FALSE, include_age = FALSE,
                            include_address = TRUE) {
  n <- 1L + n_comps  # subject + comps
  df <- data.frame(
    sale_price = c(NA, rep(300000, n_comps) + seq_len(n_comps) * 10000),
    sale_age = c(0L, seq_len(n_comps) * 30L),
    basis = rep(250000, n),
    sqft_contribution = c(30000, rep(28000, n_comps)),
    sqft_adjustment = c(0, rep(2000, n_comps)),
    residual = c(5000, rep(2000, n_comps)),
    cqa = c(5.00, seq(3, 8, length.out = n_comps)),
    subject_cqa = c(5.00, rep(NA, n_comps)),
    net_adjustments = c(NA, rep(5000, n_comps)),
    gross_adjustments = c(NA, rep(7000, n_comps)),
    stringsAsFactors = FALSE
  )

  if (include_address) {
    df$street_address <- c("123 Subject St",
                            paste0(seq_len(n_comps) * 100, " Comp Ave"))
    df$city_name <- rep("Testville", n)
    df$postal_code <- rep("12345", n)
    df$parcel_number <- paste0("APN-", seq_len(n))
    df$listing_id <- paste0("MLS-", seq_len(n))
  }

  if (include_location) {
    df$latitude <- c(37.77, rep(37.78, n_comps) + seq_len(n_comps) * 0.01)
    df$longitude <- c(-122.42, rep(-122.41, n_comps) - seq_len(n_comps) * 0.01)
    df$latitude_contribution <- c(1000, rep(900, n_comps))
    df$latitude_adjustment <- c(0, rep(100, n_comps))
    df$longitude_contribution <- c(-500, rep(-400, n_comps))
    df$longitude_adjustment <- c(0, rep(-100, n_comps))
  }

  if (include_site) {
    df$lot_size <- c(5000, rep(4500, n_comps) + seq_len(n_comps) * 200)
    df$lot_size_contribution <- c(8000, rep(7000, n_comps))
    df$lot_size_adjustment <- c(0, rep(1000, n_comps))
  }

  if (include_age) {
    df$actual_age <- c(10L, seq_len(n_comps) * 5L + 10L)
    df$effective_age <- c(8L, seq_len(n_comps) * 4L + 8L)
    df$actual_age_contribution <- c(-2000, rep(-3000, n_comps))
    df$actual_age_adjustment <- c(0, rep(1000, n_comps))
  }

  df
}

# Helper to write df, generate grid, return sheet names.
# Caller is responsible for cleanup via withr::defer or on.exit.
run_grid <- function(df, comp_rows, specials = list(), ...) {
  tmp_in <- tempfile(fileext = ".xlsx")
  tmp_out <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(df, tmp_in)

  sg_env$generate_sales_grid(
    adjusted_file = tmp_in,
    comp_rows = comp_rows,
    output_file = tmp_out,
    specials = specials,
    ...
  )
  unlink(tmp_in)

  list(
    path = tmp_out,
    exists = file.exists(tmp_out),
    sheets = if (file.exists(tmp_out)) openxlsx::getSheetNames(tmp_out)
             else character(0),
    cleanup = function() unlink(tmp_out)
  )
}

skip_if_no_grid <- function() {
  skip_if_no_sg()
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
}

# --- Single comp (minimum viable) ---
test_that("generate_sales_grid works with single comp", {
  skip_if_no_grid()
  df <- build_test_rca(1)
  result <- run_grid(df, comp_rows = 2)
  expect_true(result$exists)
  expect_equal(length(result$sheets), 1)
})

# --- Maximum comps: 30 comps = 10 sheets ---
test_that("generate_sales_grid creates 10 sheets for 30 comps", {
  skip_if_no_grid()
  df <- build_test_rca(30)
  result <- run_grid(df, comp_rows = 2:31)
  expect_true(result$exists)
  expect_equal(length(result$sheets), 10)
})

# --- Partial last sheet (e.g., 5 comps = 2 sheets, last has 2) ---
test_that("generate_sales_grid handles partial last sheet", {
  skip_if_no_grid()
  df <- build_test_rca(5)
  result <- run_grid(df, comp_rows = 2:6)
  expect_true(result$exists)
  expect_equal(length(result$sheets), 2)
})

# --- Comps with missing address columns ---
test_that("generate_sales_grid works without address columns", {
  skip_if_no_grid()
  df <- build_test_rca(3, include_address = FALSE)
  result <- run_grid(df, comp_rows = c(2, 3, 4))
  expect_true(result$exists)
  expect_equal(length(result$sheets), 1)
})

# --- Comps with missing sale_price ---
test_that("generate_sales_grid handles NA sale_price", {
  skip_if_no_grid()
  df <- build_test_rca(3)
  df$sale_price[3] <- NA  # comp 2 has no sale price
  result <- run_grid(df, comp_rows = c(2, 3, 4))
  expect_true(result$exists)
})

# --- Grouped rows: location ---
test_that("generate_sales_grid includes location grouped row", {
  skip_if_no_grid()
  df <- build_test_rca(3, include_location = TRUE)
  specials <- list(latitude = "latitude", longitude = "longitude")
  result <- run_grid(df, comp_rows = c(2, 3, 4), specials = specials)
  expect_true(result$exists)

  # Read back and check the location row is present
  wb <- openxlsx::loadWorkbook(result$path)
  sheet_data <- openxlsx::read.xlsx(wb, sheet = 1, colNames = FALSE,
                                     skipEmptyRows = FALSE)
  # Find row containing "Loc:"
  loc_rows <- which(grepl("Loc:", sheet_data[[1]], fixed = TRUE))
  expect_true(length(loc_rows) > 0)
})

# --- Grouped rows: site ---
test_that("generate_sales_grid includes site grouped row", {
  skip_if_no_grid()
  df <- build_test_rca(3, include_site = TRUE)
  specials <- list(lot_size = "lot_size")
  result <- run_grid(df, comp_rows = c(2, 3, 4), specials = specials)
  expect_true(result$exists)

  wb <- openxlsx::loadWorkbook(result$path)
  sheet_data <- openxlsx::read.xlsx(wb, sheet = 1, colNames = FALSE,
                                     skipEmptyRows = FALSE)
  site_rows <- which(grepl("Site", sheet_data[[1]], fixed = TRUE))
  expect_true(length(site_rows) > 0)
})

# --- Grouped rows: age ---
test_that("generate_sales_grid includes age grouped row", {
  skip_if_no_grid()
  df <- build_test_rca(3, include_age = TRUE)
  specials <- list(actual_age = "actual_age", effective_age = "effective_age")
  result <- run_grid(df, comp_rows = c(2, 3, 4), specials = specials)
  expect_true(result$exists)

  wb <- openxlsx::loadWorkbook(result$path)
  sheet_data <- openxlsx::read.xlsx(wb, sheet = 1, colNames = FALSE,
                                     skipEmptyRows = FALSE)
  age_rows <- which(grepl("Age", sheet_data[[1]], fixed = TRUE))
  expect_true(length(age_rows) > 0)
})

# --- All grouped rows together ---
test_that("generate_sales_grid includes all grouped rows", {
  skip_if_no_grid()
  df <- build_test_rca(3, include_location = TRUE, include_site = TRUE,
                        include_age = TRUE)
  specials <- list(
    latitude = "latitude", longitude = "longitude",
    lot_size = "lot_size",
    actual_age = "actual_age", effective_age = "effective_age"
  )
  result <- run_grid(df, comp_rows = c(2, 3, 4), specials = specials)
  expect_true(result$exists)

  wb <- openxlsx::loadWorkbook(result$path)
  sheet_data <- openxlsx::read.xlsx(wb, sheet = 1, colNames = FALSE,
                                     skipEmptyRows = FALSE)
  expect_true(any(grepl("Loc:", sheet_data[[1]], fixed = TRUE)))
  expect_true(any(grepl("Site", sheet_data[[1]], fixed = TRUE)))
  expect_true(any(grepl("Age", sheet_data[[1]], fixed = TRUE)))
})

# --- Validation: comp_rows out of range ---
test_that("generate_sales_grid errors on out-of-range comp_rows", {
  skip_if_no_grid()
  df <- build_test_rca(3)
  tmp_in <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(df, tmp_in)
  on.exit(unlink(tmp_in), add = TRUE)

  expect_error(
    sg_env$generate_sales_grid(tmp_in, comp_rows = c(1)),
    "comp_rows must be between 2"
  )
  expect_error(
    sg_env$generate_sales_grid(tmp_in, comp_rows = c(100)),
    "comp_rows must be between 2"
  )
})

# --- Validation: more than 30 comps ---
test_that("generate_sales_grid errors on more than 30 comps", {
  skip_if_no_grid()
  df <- build_test_rca(35)
  tmp_in <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(df, tmp_in)
  on.exit(unlink(tmp_in), add = TRUE)

  expect_error(
    sg_env$generate_sales_grid(tmp_in, comp_rows = 2:36),
    "Maximum 30 comps"
  )
})

# --- Validation: missing file ---
test_that("generate_sales_grid errors on missing file", {
  skip_if_no_sg()
  expect_error(
    sg_env$generate_sales_grid("/nonexistent/file.xlsx", comp_rows = c(2)),
    "not found"
  )
})

# --- Default output_file ---
test_that("generate_sales_grid auto-generates output filename", {
  skip_if_no_grid()
  df <- build_test_rca(2)
  tmp_in <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(df, tmp_in)
  on.exit(unlink(tmp_in), add = TRUE)

  result <- sg_env$generate_sales_grid(tmp_in, comp_rows = c(2, 3))
  on.exit(unlink(result), add = TRUE)
  expect_true(file.exists(result))
  expect_match(basename(result), "salesgrid")
})

# --- Progress callback ---
test_that("generate_sales_grid calls progress_fn", {
  skip_if_no_grid()
  df <- build_test_rca(6)  # 2 sheets

  progress_calls <- list()
  mock_progress <- function(sheet, total_sheets, comps_done, total_comps) {
    progress_calls[[length(progress_calls) + 1L]] <<- list(
      sheet = sheet, total_sheets = total_sheets,
      comps_done = comps_done, total_comps = total_comps
    )
  }

  result <- run_grid(df, comp_rows = 2:7, progress_fn = mock_progress)
  expect_true(result$exists)
  expect_equal(length(progress_calls), 2)  # 2 sheets
  expect_equal(progress_calls[[1]]$sheet, 1)
  expect_equal(progress_calls[[2]]$sheet, 2)
  expect_equal(progress_calls[[2]]$total_comps, 6)
})

# --- Non-contiguous comp_rows ---
test_that("generate_sales_grid handles non-contiguous comp selection", {
  skip_if_no_grid()
  df <- build_test_rca(10)
  # Select only rows 2, 5, 8 (non-contiguous)
  result <- run_grid(df, comp_rows = c(2, 5, 8))
  expect_true(result$exists)
  expect_equal(length(result$sheets), 1)
})

# --- Data with no model variables ---
test_that("generate_sales_grid handles data with no model variables", {
  skip_if_no_grid()
  # Minimal df without any _contribution/_adjustment columns
  df <- data.frame(
    sale_price = c(NA, 300000, 310000),
    basis = rep(250000, 3),
    residual = c(5000, 2000, -3000),
    cqa = c(5.00, 6.50, 3.20),
    subject_cqa = c(5.00, NA, NA),
    net_adjustments = c(NA, 7000, 5000),
    gross_adjustments = c(NA, 7000, 15000),
    stringsAsFactors = FALSE
  )
  result <- run_grid(df, comp_rows = c(2, 3))
  expect_true(result$exists)
})

# --- Copyright branding ---
test_that("generate_sales_grid uses glmnetUI branding", {
  skip_if_no_grid()
  df <- build_test_rca(1)
  result <- run_grid(df, comp_rows = 2)
  expect_true(result$exists)

  wb <- openxlsx::loadWorkbook(result$path)
  sheet_data <- openxlsx::read.xlsx(wb, sheet = 1, colNames = FALSE,
                                     skipEmptyRows = FALSE)
  # Find copyright row
  copyright_rows <- which(grepl("glmnetUI", sheet_data[[1]], fixed = TRUE))
  expect_true(length(copyright_rows) > 0)
  # Should NOT say earthUI
  earth_rows <- which(grepl("earthUI", sheet_data[[1]], fixed = TRUE))
  expect_equal(length(earth_rows), 0)
})

# --- Cell formatting tests ---

# Helper: extract numFmt for a specific cell from an openxlsx workbook.
# sheet can be a number (index) or name.
get_cell_numfmt <- function(wb, sheet, row, col) {
  # Resolve sheet name
  sheet_name <- if (is.numeric(sheet)) {
    wb$sheet_names[sheet]
  } else {
    sheet
  }
  for (so in wb$styleObjects) {
    if (identical(so$sheet, sheet_name) &&
        row %in% so$rows &&
        col %in% so$cols) {
      nf <- so$style$numFmt
      if (!is.null(nf)) {
        if (!is.null(nf$formatCode)) return(nf$formatCode)
        if (!is.null(nf$numFmtId)) return(paste0("builtin:", nf$numFmtId))
      }
    }
  }
  "GENERAL"
}

test_that("sales grid value contribution cells have number format", {
  skip_if_no_grid()
  df <- build_test_rca(3)
  result <- run_grid(df, comp_rows = 2:4)
  expect_true(result$exists)
  on.exit(result$cleanup())

  wb <- openxlsx::loadWorkbook(result$path)

  # Read sheet data to find model variable rows
  sheet_data <- openxlsx::read.xlsx(wb, sheet = 1, colNames = FALSE,
                                     skipEmptyRows = FALSE)

  # Find the BASE VALUE row
  base_row <- which(sheet_data[[1]] == "BASE VALUE")
  expect_true(length(base_row) == 1,
              info = "Should find BASE VALUE row")

  # Subject VC is in col 5 (E), comp 1 VC is col 9 (I)
  # Check that these cells don't have date format
  base_fmt <- get_cell_numfmt(wb, 1, base_row, 5)
  expect_true(grepl("#|0", base_fmt),
              info = paste("BASE VALUE VC should be number format, got:", base_fmt))

  # Find the sqft model variable row (first after BASE VALUE)
  sqft_label_rows <- which(grepl("sqft|Sqft|SF", sheet_data[[1]],
                                  ignore.case = TRUE))
  # Filter to rows after base_row
  sqft_rows <- sqft_label_rows[sqft_label_rows > base_row]
  if (length(sqft_rows) > 0) {
    rw <- sqft_rows[1]
    # Subject VC col 5
    vc_fmt <- get_cell_numfmt(wb, 1, rw, 5)
    expect_true(grepl("#|0", vc_fmt),
                info = paste("sqft VC should be number format, got:", vc_fmt))

    # Comp 1 VC col 9, Adjustment col 10
    comp_vc_fmt <- get_cell_numfmt(wb, 1, rw, 9)
    comp_adj_fmt <- get_cell_numfmt(wb, 1, rw, 10)
    expect_true(grepl("#|0", comp_vc_fmt),
                info = paste("comp VC should be number format, got:", comp_vc_fmt))
    expect_true(grepl("#|0", comp_adj_fmt),
                info = paste("comp adj should be number format, got:", comp_adj_fmt))
  }
})

test_that("sales grid sale price cells have number format", {
  skip_if_no_grid()
  df <- build_test_rca(3)
  result <- run_grid(df, comp_rows = 2:4)
  expect_true(result$exists)
  on.exit(result$cleanup())

  wb <- openxlsx::loadWorkbook(result$path)
  sheet_data <- openxlsx::read.xlsx(wb, sheet = 1, colNames = FALSE,
                                     skipEmptyRows = FALSE)

  # Find Sale Price row
  sp_row <- which(grepl("Sales Price", sheet_data[[1]], fixed = TRUE))
  expect_true(length(sp_row) == 1)

  # Comp 1 sale price is in col 6
  sp_fmt <- get_cell_numfmt(wb, 1, sp_row, 6)
  expect_true(grepl("#|0", sp_fmt),
              info = paste("Sale price should be number format, got:", sp_fmt))
})

test_that("sales grid grouped row cells have number format", {
  skip_if_no_grid()
  df <- build_test_rca(3, include_location = TRUE)
  result <- run_grid(df, comp_rows = 2:4,
                     specials = list(latitude = "latitude",
                                     longitude = "longitude"))
  expect_true(result$exists)
  on.exit(result$cleanup())

  wb <- openxlsx::loadWorkbook(result$path)
  sheet_data <- openxlsx::read.xlsx(wb, sheet = 1, colNames = FALSE,
                                     skipEmptyRows = FALSE)

  # Find Loc row
  loc_row <- which(grepl("Loc:", sheet_data[[1]], fixed = TRUE))
  expect_true(length(loc_row) == 1,
              info = "Should find Loc row")

  # Subject combined VC col 5
  loc_fmt <- get_cell_numfmt(wb, 1, loc_row, 5)
  expect_true(grepl("#|0", loc_fmt),
              info = paste("Loc VC should be number format, got:", loc_fmt))

  # Comp 1 adjustment col 10
  loc_adj_fmt <- get_cell_numfmt(wb, 1, loc_row, 10)
  expect_true(grepl("#|0", loc_adj_fmt),
              info = paste("Loc adj should be number format, got:", loc_adj_fmt))
})

test_that("sales grid percentage cells have percent format", {
  skip_if_no_grid()
  df <- build_test_rca(3)
  result <- run_grid(df, comp_rows = 2:4)
  expect_true(result$exists)
  on.exit(result$cleanup())

  wb <- openxlsx::loadWorkbook(result$path)
  sheet_data <- openxlsx::read.xlsx(wb, sheet = 1, colNames = FALSE,
                                     skipEmptyRows = FALSE)

  # Find Net Adjustment % row
  net_pct_row <- which(grepl("Net Adjustment %", sheet_data[[1]],
                              fixed = TRUE))
  expect_true(length(net_pct_row) == 1)

  # Comp 1 net adj % col 10
  pct_fmt <- get_cell_numfmt(wb, 1, net_pct_row, 10)
  expect_true(grepl("%", pct_fmt),
              info = paste("Net adj % should be percent format, got:", pct_fmt))

  # Find Gross Adjustment % row
  gross_pct_row <- which(grepl("Gross Adjustment %", sheet_data[[1]],
                                fixed = TRUE))
  expect_true(length(gross_pct_row) == 1)

  gross_fmt <- get_cell_numfmt(wb, 1, gross_pct_row, 10)
  expect_true(grepl("%", gross_fmt),
              info = paste("Gross adj % should be percent format, got:", gross_fmt))
})

test_that("sales grid adjusted sale price has number format", {
  skip_if_no_grid()
  df <- build_test_rca(3)
  result <- run_grid(df, comp_rows = 2:4)
  expect_true(result$exists)
  on.exit(result$cleanup())

  wb <- openxlsx::loadWorkbook(result$path)
  sheet_data <- openxlsx::read.xlsx(wb, sheet = 1, colNames = FALSE,
                                     skipEmptyRows = FALSE)

  asp_row <- which(grepl("Adjusted Sale Price", sheet_data[[1]],
                          fixed = TRUE))
  expect_true(length(asp_row) == 1)

  # Comp 1 adjusted SP (merged cols 6-10, style on col 6)
  asp_fmt <- get_cell_numfmt(wb, 1, asp_row, 6)
  expect_true(grepl("#|0", asp_fmt),
              info = paste("Adj SP should be number format, got:", asp_fmt))
})

test_that("sales grid formats correctly when input has Date/POSIXct columns", {
  skip_if_no_grid()

  # Build RCA data with Date columns (simulates auto_parse_dates_ output)
  df <- build_test_rca(3)
  df$contract_date <- as.POSIXct(c("2025-01-15", "2025-02-20",
                                     "2025-03-10", "2025-04-05"))
  df$listing_date <- as.Date(c("2024-11-01", "2024-12-15",
                                 "2025-01-05", "2025-02-10"))

  result <- run_grid(df, comp_rows = 2:4)
  expect_true(result$exists)
  on.exit(result$cleanup())

  wb <- openxlsx::loadWorkbook(result$path)
  sheet_data <- openxlsx::read.xlsx(wb, sheet = 1, colNames = FALSE,
                                     skipEmptyRows = FALSE)

  # BASE VALUE VC should still be number format, not date
  base_row <- which(sheet_data[[1]] == "BASE VALUE")
  expect_true(length(base_row) == 1)
  base_fmt <- get_cell_numfmt(wb, 1, base_row, 5)
  expect_true(grepl("#|0", base_fmt),
              info = paste("BASE VALUE with date cols should be number, got:", base_fmt))

  # Sale price should be number format
  sp_row <- which(grepl("Sales Price", sheet_data[[1]], fixed = TRUE))
  expect_true(length(sp_row) == 1)
  sp_fmt <- get_cell_numfmt(wb, 1, sp_row, 6)
  expect_true(grepl("#|0", sp_fmt),
              info = paste("Sale price with date cols should be number, got:", sp_fmt))

  # Sqft contribution should be number format
  sqft_rows <- which(grepl("sqft|Sqft|SF", sheet_data[[1]], ignore.case = TRUE))
  sqft_rows <- sqft_rows[sqft_rows > base_row]
  if (length(sqft_rows) > 0) {
    vc_fmt <- get_cell_numfmt(wb, 1, sqft_rows[1], 5)
    expect_true(grepl("#|0", vc_fmt),
                info = paste("sqft VC with date cols should be number, got:", vc_fmt))
  }
})
