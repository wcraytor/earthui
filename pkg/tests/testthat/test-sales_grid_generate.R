# test-sales_grid_generate.R — Integration tests for generate_sales_grid()
#
# These tests create a minimal adjusted Excel file and verify
# the generated Sales Grid workbook structure.

skip_if_not_installed("openxlsx")
skip_if_not_installed("readxl")
skip_if_not_installed("writexl")

# Source the sales grid generator
sg_path <- system.file("app", "sales_grid.R", package = "earthUI")
skip_if(!nzchar(sg_path), "sales_grid.R not found in installed package")

sg_env <- new.env(parent = globalenv())
suppressPackageStartupMessages(source(sg_path, local = sg_env))
generate_sales_grid <- sg_env$generate_sales_grid

# --- Helper: create a minimal adjusted file mimicking RCA output ---
create_test_adjusted <- function(n_comps = 6, with_specials = TRUE) {
  n <- n_comps + 1  # row 1 = subject
  df <- data.frame(
    street_address  = paste("Address", seq_len(n)),
    city_name       = rep("TestCity", n),
    postal_code     = rep("95000", n),
    parcel_number   = paste0("APN-", seq_len(n)),
    listing_id      = paste0("MLS-", seq_len(n)),
    sale_price      = c(NA, rep(500000, n_comps)),
    sale_age        = c(0, seq_len(n_comps) * 30),
    living_sqft     = c(2000, rep(1800, n_comps)),
    lot_size        = c(7000, rep(6500, n_comps)),
    age             = c(20, rep(25, n_comps)),
    latitude        = c(38.500, seq(38.501, 38.500 + n_comps * 0.001, length.out = n_comps)),
    longitude       = c(-121.400, seq(-121.401, -121.400 - n_comps * 0.001, length.out = n_comps)),
    area_id         = c(1, rep(2, n_comps)),
    contract_date   = as.character(Sys.Date() - c(0, seq_len(n_comps) * 30)),
    listing_date    = as.character(Sys.Date() - c(0, seq_len(n_comps) * 30) - 30),
    days_on_market  = c(30, rep(45, n_comps)),
    sale_concessions = c(0, rep(1000, n_comps)),
    # Model columns
    basis           = rep(300000, n),
    sale_age_contribution   = c(5000, seq_len(n_comps) * 1000),
    sale_age_adjustment     = c(0, 5000 - seq_len(n_comps) * 1000),
    living_sqft_contribution = c(80000, rep(72000, n_comps)),
    living_sqft_adjustment   = c(0, rep(8000, n_comps)),
    latitude_contribution    = c(10000, rep(9000, n_comps)),
    latitude_adjustment      = c(0, rep(1000, n_comps)),
    longitude_contribution   = c(5000, rep(4500, n_comps)),
    longitude_adjustment     = c(0, rep(500, n_comps)),
    lot_size_contribution    = c(15000, rep(14000, n_comps)),
    lot_size_adjustment      = c(0, rep(1000, n_comps)),
    age_contribution         = c(-8000, rep(-10000, n_comps)),
    age_adjustment           = c(0, rep(2000, n_comps)),
    # RCA columns
    residual         = c(5000, rnorm(n_comps, 0, 5000)),
    cqa              = c(5.50, runif(n_comps, 0, 10)),
    residual_sf      = c(2.50, rnorm(n_comps, 0, 2)),
    cqa_sf           = c(5.25, runif(n_comps, 0, 10)),
    subject_value    = c(510000, rep(NA, n_comps)),
    subject_cqa      = c(5.50, rep(NA, n_comps)),
    net_adjustments  = c(0, round(rnorm(n_comps, 0, 15000))),
    gross_adjustments = c(0, round(abs(rnorm(n_comps, 20000, 10000)))),
    adjusted_sale_price = c(510000, rep(500000, n_comps) + round(rnorm(n_comps, 0, 15000))),
    stringsAsFactors = FALSE
  )
  # Write to temp Excel file
  tmp <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(df, tmp)
  tmp
}

# --- Test: basic generation ---
test_that("generate_sales_grid creates output file", {
  adj_file <- create_test_adjusted(n_comps = 3)
  on.exit(unlink(adj_file))
  out_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out_file), add = TRUE)

  specials <- list(
    latitude = "latitude", longitude = "longitude", area = "area_id",
    lot_size = "lot_size", actual_age = "age",
    contract_date = "contract_date", dom = "days_on_market",
    concessions = "sale_concessions", living_area = "living_sqft"
  )

  result <- generate_sales_grid(adj_file, comp_rows = c(2, 3, 4),
                                output_file = out_file, specials = specials)
  expect_true(file.exists(out_file))
  expect_equal(result, out_file)
})

test_that("generate_sales_grid creates correct number of sheets", {
  adj_file <- create_test_adjusted(n_comps = 6)
  on.exit(unlink(adj_file))
  out_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out_file), add = TRUE)

  generate_sales_grid(adj_file, comp_rows = c(2, 3, 4, 5, 6, 7),
                      output_file = out_file)
  wb <- openxlsx::loadWorkbook(out_file)
  expect_equal(length(names(wb)), 2)  # 6 comps / 3 per sheet = 2 sheets
})

test_that("generate_sales_grid with single comp works", {
  adj_file <- create_test_adjusted(n_comps = 3)
  on.exit(unlink(adj_file))
  out_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out_file), add = TRUE)

  generate_sales_grid(adj_file, comp_rows = 2, output_file = out_file)
  expect_true(file.exists(out_file))
  wb <- openxlsx::loadWorkbook(out_file)
  expect_equal(length(names(wb)), 1)
})

# --- Test: validation ---
test_that("generate_sales_grid errors on missing file", {
  expect_error(generate_sales_grid("nonexistent.xlsx", comp_rows = 2),
               "not found")
})

test_that("generate_sales_grid errors on invalid comp_rows", {
  adj_file <- create_test_adjusted(n_comps = 3)
  on.exit(unlink(adj_file))

  expect_error(generate_sales_grid(adj_file, comp_rows = c(1)),
               "must be between")
  expect_error(generate_sales_grid(adj_file, comp_rows = c(100)),
               "must be between")
})

test_that("generate_sales_grid errors on too many comps", {
  adj_file <- create_test_adjusted(n_comps = 35)
  on.exit(unlink(adj_file))

  expect_error(generate_sales_grid(adj_file, comp_rows = 2:32),
               "Maximum 30")
})

# --- Test: grouped rows ---
test_that("generate_sales_grid includes grouped rows when specials present", {
  adj_file <- create_test_adjusted(n_comps = 3)
  on.exit(unlink(adj_file))
  out_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out_file), add = TRUE)

  specials <- list(
    latitude = "latitude", longitude = "longitude", area = "area_id",
    lot_size = "lot_size", actual_age = "age"
  )
  generate_sales_grid(adj_file, comp_rows = c(2, 3, 4),
                      output_file = out_file, specials = specials)

  # Read back and check for grouped row labels
  wb <- openxlsx::loadWorkbook(out_file)
  sheet_data <- openxlsx::read.xlsx(wb, sheet = 1, colNames = FALSE,
                                     skipEmptyRows = FALSE)
  labels <- sheet_data[[1]]
  expect_true(any(grepl("Loc: Long", labels, fixed = TRUE)))
  expect_true(any(grepl("Site Size", labels, fixed = TRUE)))
  expect_true(any(grepl("Actual Age", labels, fixed = TRUE)))
})

test_that("generate_sales_grid omits grouped rows when no specials", {
  adj_file <- create_test_adjusted(n_comps = 3)
  on.exit(unlink(adj_file))
  out_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out_file), add = TRUE)

  # No specials = no grouped rows
  generate_sales_grid(adj_file, comp_rows = c(2, 3, 4),
                      output_file = out_file, specials = list())

  wb <- openxlsx::loadWorkbook(out_file)
  sheet_data <- openxlsx::read.xlsx(wb, sheet = 1, colNames = FALSE,
                                     skipEmptyRows = FALSE)
  labels <- sheet_data[[1]]
  expect_false(any(grepl("Loc: Long", labels, fixed = TRUE)))
  expect_false(any(grepl("Site Size", labels, fixed = TRUE)))
})

# --- Test: progress callback ---
test_that("generate_sales_grid calls progress function", {
  adj_file <- create_test_adjusted(n_comps = 3)
  on.exit(unlink(adj_file))
  out_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out_file), add = TRUE)

  progress_log <- list()
  pfn <- function(...) {
    progress_log[[length(progress_log) + 1L]] <<- list(...)
  }

  generate_sales_grid(adj_file, comp_rows = c(2, 3, 4),
                      output_file = out_file, progress_fn = pfn)
  expect_length(progress_log, 1)
  expect_equal(progress_log[[1]]$sheet, 1)
  expect_equal(progress_log[[1]]$total_sheets, 1)
})

# --- Test: default output file ---
test_that("generate_sales_grid generates default filename", {
  adj_file <- create_test_adjusted(n_comps = 3)
  on.exit(unlink(adj_file))

  result <- generate_sales_grid(adj_file, comp_rows = c(2, 3, 4))
  on.exit(unlink(result), add = TRUE)

  expect_true(file.exists(result))
  expect_true(grepl("salesgrid", basename(result)))
})
