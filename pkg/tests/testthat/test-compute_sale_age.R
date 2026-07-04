# --- compute_sale_age (sale age in integer days) ----------------------------

test_that("compute_sale_age handles Date, ISO character, and Excel serial", {
  eff <- as.Date("2026-01-01")

  expect_equal(compute_sale_age(as.Date(c("2025-12-18", "2025-06-01")), eff),
               c(14L, 214L))
  expect_equal(compute_sale_age(c("2025-12-18", "2025-06-01"), eff),
               c(14L, 214L))
  serial <- as.numeric(as.Date("2025-12-18") - as.Date("1899-12-30"))
  expect_equal(compute_sale_age(serial, eff), 14L)
})

test_that("compute_sale_age parses non-ISO character dates (US format)", {
  eff <- as.Date("2026-01-01")

  # Four-digit mm/dd/YYYY — previously errored with "not in a standard
  # unambiguous format" because bare as.POSIXct() only accepts ISO
  expect_equal(compute_sale_age(c("12/18/2025", "06/01/2025"), eff),
               c(14L, 214L))
  # Two-digit years go through the floating window (25 -> 2025 today)
  expect_equal(compute_sale_age(c("12/18/25", "06/01/25"), eff),
               c(14L, 214L))
  # NA contract values propagate
  expect_equal(compute_sale_age(c("12/18/25", NA), eff), c(14L, NA_integer_))
})

test_that("compute_sale_age rejects unparseable input clearly", {
  eff <- as.Date("2026-01-01")
  expect_error(compute_sale_age(c("foo", "bar"), eff), "Cannot parse")
  expect_error(compute_sale_age(NULL, eff), "must not be NULL")
  expect_error(compute_sale_age(as.Date("2025-12-18"), NULL), "must not be NULL")
})
