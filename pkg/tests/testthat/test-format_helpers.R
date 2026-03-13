# Tests for internal formatting helpers in plot_results.R and format_results.R

# -- auto_digits_ --------------------------------------------------------------

test_that("auto_digits_ returns 3 for range < 1 (lat/long scale)", {
  # e.g. longitude values

  x <- c(-122.400, -122.385)
  expect_equal(earthUI:::auto_digits_(x), 3L)
})

test_that("auto_digits_ returns 2 for range < 10", {
  x <- c(1.0, 5.5)
  expect_equal(earthUI:::auto_digits_(x), 2L)
})

test_that("auto_digits_ returns 1 for range < 100", {
  x <- c(10, 60)
  expect_equal(earthUI:::auto_digits_(x), 1L)
})

test_that("auto_digits_ returns 0 for range >= 100", {
  x <- c(200000, 500000)
  expect_equal(earthUI:::auto_digits_(x), 0L)
})

test_that("auto_digits_ returns 0 for fewer than 2 finite values", {
  expect_equal(earthUI:::auto_digits_(c(42)), 0L)
  expect_equal(earthUI:::auto_digits_(numeric(0)), 0L)
  expect_equal(earthUI:::auto_digits_(c(NA, Inf)), 0L)
})

test_that("auto_digits_ ignores non-finite values", {
  x <- c(NA, 10, 60, Inf, -Inf, NaN)
  expect_equal(earthUI:::auto_digits_(x), 1L)  # range of c(10,60) = 50
})

test_that("auto_digits_ boundary: range exactly 1", {
  x <- c(0, 1)  # range = 1, not < 1
  expect_equal(earthUI:::auto_digits_(x), 2L)
})

test_that("auto_digits_ boundary: range exactly 10", {
  x <- c(0, 10)
  expect_equal(earthUI:::auto_digits_(x), 1L)  # 10 is not < 10, but < 100
})

test_that("auto_digits_ boundary: range exactly 100", {
  x <- c(0, 100)
  expect_equal(earthUI:::auto_digits_(x), 0L)
})

# -- format_slope_labels_ ------------------------------------------------------

test_that("format_slope_labels_ uses /unit for large-range breaks", {
  labels <- earthUI:::format_slope_labels_(c(100, -50), c(200000, 500000))
  expect_true(all(grepl("/unit$", labels)))
  expect_match(labels[1], "^\\+")
  expect_match(labels[2], "^-")
})

test_that("format_slope_labels_ uses /0.001 for lat/long-range breaks", {
  labels <- earthUI:::format_slope_labels_(c(5000), c(-122.40, -122.38))
  expect_match(labels[1], "/0.001$")
})

test_that("format_slope_labels_ uses /0.01 for small-range breaks", {
  labels <- earthUI:::format_slope_labels_(c(200), c(1.0, 5.5))
  expect_match(labels[1], "/0.01$")
})

# -- dollar_format_ ------------------------------------------------------------

test_that("dollar_format_ formats positive values with locale separators", {
  result <- earthUI:::dollar_format_(c(200000, 350000))
  expect_match(result[1], "^200,000$")
  expect_match(result[2], "^350,000$")
})

test_that("dollar_format_ formats negative values", {
  result <- earthUI:::dollar_format_(c(-100000, 200000))
  expect_match(result[1], "^-100,000$")
})

test_that("dollar_format_ returns empty string for NA", {
  result <- earthUI:::dollar_format_(c(NA, 100))
  expect_equal(result[1], "")
})

test_that("dollar_format_ adapts decimal places to range", {
  # Small range (lat/long): 3 decimal places
  result <- earthUI:::dollar_format_(c(-122.400, -122.385))
  expect_match(result[1], "122\\.400$")
})

# -- comma_format_ -------------------------------------------------------------

test_that("comma_format_ formats with commas, no dollar sign", {
  result <- earthUI:::comma_format_(c(1234567, 89012))
  expect_match(result[1], "^1,234,567$")
  expect_match(result[2], "^89,012$")
})

test_that("comma_format_ returns empty string for NA", {
  result <- earthUI:::comma_format_(c(NA, 100))
  expect_equal(result[1], "")
})

test_that("comma_format_ adapts decimal places to range", {
  result <- earthUI:::comma_format_(c(1.1, 5.5))
  expect_match(result[1], "1\\.10$")  # 2 dp for range < 10
})

# -- locale formatting ---------------------------------------------------------

test_that("German locale uses period as big mark and comma as decimal", {
  earthUI:::set_locale_("de")
  on.exit(earthUI:::set_locale_("us"))
  result <- earthUI:::dollar_format_(c(200000, 350000))
  expect_match(result[1], "^200\\.000$")
  expect_match(result[2], "^350\\.000$")
})

test_that("Finnish locale uses space as big mark", {
  earthUI:::set_locale_("fi")
  on.exit(earthUI:::set_locale_("us"))
  result <- earthUI:::dollar_format_(c(200000, 350000))
  expect_match(result[1], "^200 000$")
})

test_that("German locale comma_format_ uses correct separators", {
  earthUI:::set_locale_("de")
  on.exit(earthUI:::set_locale_("us"))
  result <- earthUI:::comma_format_(c(1.1, 5.5))
  expect_match(result[1], "1,10$")
})

test_that("locale helpers return correct values for US", {
  earthUI:::set_locale_("us")
  expect_equal(earthUI:::locale_csv_sep_(), ",")
  expect_equal(earthUI:::locale_csv_dec_(), ".")
  expect_equal(earthUI:::locale_big_mark_(), ",")
  expect_equal(earthUI:::locale_paper_(), "letter")
})

test_that("locale helpers return correct values for Germany", {
  earthUI:::set_locale_("de")
  on.exit(earthUI:::set_locale_("us"))
  expect_equal(earthUI:::locale_csv_sep_(), ";")
  expect_equal(earthUI:::locale_csv_dec_(), ",")
  expect_equal(earthUI:::locale_big_mark_(), ".")
  expect_equal(earthUI:::locale_paper_(), "a4")
})

test_that("locale helpers return correct values for Finland", {
  earthUI:::set_locale_("fi")
  on.exit(earthUI:::set_locale_("us"))
  expect_equal(earthUI:::locale_csv_sep_(), ";")
  expect_equal(earthUI:::locale_csv_dec_(), ",")
  expect_equal(earthUI:::locale_big_mark_(), " ")
  expect_equal(earthUI:::locale_paper_(), "a4")
})

test_that("locale date formats prioritize locale-preferred order", {
  earthUI:::set_locale_("de")
  on.exit(earthUI:::set_locale_("us"))
  fmts <- earthUI:::locale_date_formats_()
  expect_equal(fmts[1], "%d/%m/%Y")  # DD/MM first for dmy

  earthUI:::set_locale_("us")
  fmts <- earthUI:::locale_date_formats_()
  expect_equal(fmts[1], "%m/%d/%Y")  # MM/DD first for mdy

  earthUI:::set_locale_("se")
  fmts <- earthUI:::locale_date_formats_()
  expect_equal(fmts[1], "%Y-%m-%d")  # ISO first for ymd
})

test_that("set_locale_ accepts overrides", {
  earthUI:::set_locale_("us", csv_sep = ";", paper = "a4")
  on.exit(earthUI:::set_locale_("us"))
  expect_equal(earthUI:::locale_csv_sep_(), ";")
  expect_equal(earthUI:::locale_paper_(), "a4")
  # Non-overridden values use US defaults
  expect_equal(earthUI:::locale_csv_dec_(), ".")
  expect_equal(earthUI:::locale_big_mark_(), ",")
})

# -- format_number_ ------------------------------------------------------------

test_that("format_number_ returns '0' for zero", {
  expect_equal(earthUI:::format_number_(0), "0")
})

test_that("format_number_ formats non-zero values", {
  result <- earthUI:::format_number_(3.14159, digits = 4L)
  expect_equal(result, "3.142")
})

# -- latex_escape_text_ --------------------------------------------------------

test_that("latex_escape_text_ escapes %, &, #", {
  expect_equal(earthUI:::latex_escape_text_("100%"), "100\\%")
  expect_equal(earthUI:::latex_escape_text_("A&B"), "A\\&B")
  expect_equal(earthUI:::latex_escape_text_("item #1"), "item \\#1")
})

test_that("latex_escape_text_ handles multiple special chars", {
  result <- earthUI:::latex_escape_text_("100% & #1")
  expect_equal(result, "100\\% \\& \\#1")
})

# -- html_escape_ --------------------------------------------------------------

test_that("html_escape_ escapes &, <, >", {
  expect_equal(earthUI:::html_escape_("A & B"), "A &amp; B")
  expect_equal(earthUI:::html_escape_("x < y"), "x &lt; y")
  expect_equal(earthUI:::html_escape_("x > y"), "x &gt; y")
})

test_that("html_escape_ handles combined HTML entities", {
  result <- earthUI:::html_escape_("if (x < 0 & y > 0)")
  expect_equal(result, "if (x &lt; 0 &amp; y &gt; 0)")
})
