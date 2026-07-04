# test-clean_names.R — Edge case tests for clean_names_

test_that("clean_names_ handles spaces", {
  result <- earthUI:::clean_names_(c("First Name", "Last Name"))
  expect_equal(result, c("first_name", "last_name"))
})

test_that("clean_names_ handles dots", {
  result <- earthUI:::clean_names_(c("sale.price", "lot.size.sqft"))
  expect_equal(result, c("sale_price", "lot_size_sqft"))
})

test_that("clean_names_ handles camelCase", {
  result <- earthUI:::clean_names_(c("salePrice", "lotSizeSqft"))
  expect_equal(result, c("sale_price", "lot_size_sqft"))
})

test_that("clean_names_ handles ALLCAPS", {
  result <- earthUI:::clean_names_(c("AGE", "PRICE"))
  expect_equal(result, c("age", "price"))
})

test_that("clean_names_ handles special characters", {
  result <- earthUI:::clean_names_(c("price($)", "area (sq ft)", "price/sqft"))
  expect_true(all(grepl("^[a-z_0-9]+$", result)))
})

test_that("clean_names_ handles leading/trailing underscores", {
  result <- earthUI:::clean_names_(c("_leading", "trailing_", "_both_"))
  expect_false(any(grepl("^_|_$", result)))
})

test_that("clean_names_ handles multiple consecutive separators", {
  result <- earthUI:::clean_names_(c("a__b", "c...d", "e   f"))
  expect_false(any(grepl("__", result)))
})

test_that("clean_names_ makes duplicates unique", {
  result <- earthUI:::clean_names_(c("X", "x", "x"))
  expect_equal(length(unique(result)), 3)
})

test_that("clean_names_ handles numeric-only names", {
  result <- earthUI:::clean_names_(c("123", "456"))
  expect_true(all(nzchar(result)))
})

test_that("clean_names_ handles empty strings", {
  result <- earthUI:::clean_names_(c("a", "", "b"))
  expect_equal(length(result), 3L)
})

test_that("clean_names_ handles mixed formats", {
  result <- earthUI:::clean_names_(c("Sale.Price", "lotSize", "NO_OF_STORIES", "age"))
  expect_equal(result, c("sale_price", "lot_size", "no_of_stories", "age"))
})
