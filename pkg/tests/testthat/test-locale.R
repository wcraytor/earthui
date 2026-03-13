# test-locale.R — Tests for locale/internationalization feature

# Reset locale to US after each test to avoid cross-contamination
withr_reset <- function() earthUI:::set_locale_("us")

# ---- Country presets ----

test_that("all country presets have required fields", {
  presets <- earthUI:::locale_country_presets_()
  required <- c("csv_sep", "csv_dec", "big_mark", "dec_mark", "date_fmt", "paper")
  for (code in names(presets)) {
    for (field in required) {
      expect_true(!is.null(presets[[code]][[field]]),
                  info = paste("Country", code, "missing field", field))
    }
  }
})

test_that("country choices match preset keys", {
  choices <- earthUI:::locale_country_choices_()
  presets <- earthUI:::locale_country_presets_()
  expect_true(all(choices %in% names(presets)))
})

test_that("all presets have valid values", {
  presets <- earthUI:::locale_country_presets_()
  for (code in names(presets)) {
    p <- presets[[code]]
    expect_true(p$csv_sep %in% c(",", ";"), info = paste(code, "csv_sep"))
    expect_true(p$csv_dec %in% c(".", ","), info = paste(code, "csv_dec"))
    expect_true(p$date_fmt %in% c("mdy", "dmy", "ymd"), info = paste(code, "date_fmt"))
    expect_true(p$paper %in% c("letter", "a4"), info = paste(code, "paper"))
    # csv_sep and csv_dec must differ
    expect_true(p$csv_sep != p$csv_dec,
                info = paste(code, "csv_sep and csv_dec must differ"))
  }
})

# ---- set_locale_ / get_locale_ ----

test_that("set_locale_ applies country defaults", {
  earthUI:::set_locale_("de")
  on.exit(withr_reset())
  loc <- earthUI:::get_locale_()
  expect_equal(loc$csv_sep, ";")
  expect_equal(loc$csv_dec, ",")
  expect_equal(loc$big_mark, ".")
  expect_equal(loc$dec_mark, ",")
  expect_equal(loc$date_fmt, "dmy")
  expect_equal(loc$paper, "a4")
})

test_that("set_locale_ applies overrides on top of country", {
  earthUI:::set_locale_("us", csv_sep = ";", paper = "a4")
  on.exit(withr_reset())
  expect_equal(earthUI:::locale_csv_sep_(), ";")
  expect_equal(earthUI:::locale_paper_(), "a4")
  # Non-overridden fields keep US defaults
  expect_equal(earthUI:::locale_csv_dec_(), ".")
  expect_equal(earthUI:::locale_big_mark_(), ",")
})

test_that("set_locale_ falls back to US for unknown country", {
  earthUI:::set_locale_("xx")
  on.exit(withr_reset())
  expect_equal(earthUI:::locale_csv_sep_(), ",")
  expect_equal(earthUI:::locale_paper_(), "letter")
})

# ---- Specific country conventions ----

test_that("US conventions are correct", {
  earthUI:::set_locale_("us")
  expect_equal(earthUI:::locale_csv_sep_(), ",")
  expect_equal(earthUI:::locale_csv_dec_(), ".")
  expect_equal(earthUI:::locale_big_mark_(), ",")
  expect_equal(earthUI:::locale_dec_mark_(), ".")
  expect_equal(earthUI:::locale_paper_(), "letter")
})

test_that("Finland uses space as thousands separator", {
  earthUI:::set_locale_("fi")
  on.exit(withr_reset())
  expect_equal(earthUI:::locale_big_mark_(), " ")
  expect_equal(earthUI:::locale_csv_sep_(), ";")
  expect_equal(earthUI:::locale_csv_dec_(), ",")
})

test_that("Switzerland uses apostrophe as thousands separator", {
  earthUI:::set_locale_("ch")
  on.exit(withr_reset())
  expect_equal(earthUI:::locale_big_mark_(), "'")
})

test_that("Lithuania uses ISO date format (ymd)", {
  earthUI:::set_locale_("lt")
  on.exit(withr_reset())
  fmts <- earthUI:::locale_date_formats_()
  expect_equal(fmts[1], "%Y-%m-%d")
})

test_that("Ukraine uses space thousands, comma decimal, dmy dates", {
  earthUI:::set_locale_("ua")
  on.exit(withr_reset())
  expect_equal(earthUI:::locale_big_mark_(), " ")
  expect_equal(earthUI:::locale_csv_dec_(), ",")
  fmts <- earthUI:::locale_date_formats_()
  expect_equal(fmts[1], "%d/%m/%Y")
})

test_that("Russia uses space thousands, semicolon CSV", {
  earthUI:::set_locale_("ru")
  on.exit(withr_reset())
  expect_equal(earthUI:::locale_big_mark_(), " ")
  expect_equal(earthUI:::locale_csv_sep_(), ";")
})

test_that("Turkey uses dot thousands, comma decimal", {
  earthUI:::set_locale_("tr")
  on.exit(withr_reset())
  expect_equal(earthUI:::locale_big_mark_(), ".")
  expect_equal(earthUI:::locale_csv_dec_(), ",")
})

test_that("Japan uses comma thousands, dot decimal, ymd dates", {
  earthUI:::set_locale_("jp")
  on.exit(withr_reset())
  expect_equal(earthUI:::locale_big_mark_(), ",")
  expect_equal(earthUI:::locale_csv_dec_(), ".")
  fmts <- earthUI:::locale_date_formats_()
  expect_equal(fmts[1], "%Y-%m-%d")
})

test_that("UK uses A4 paper but comma thousands like US", {
  earthUI:::set_locale_("gb")
  on.exit(withr_reset())
  expect_equal(earthUI:::locale_paper_(), "a4")
  expect_equal(earthUI:::locale_big_mark_(), ",")
  expect_equal(earthUI:::locale_csv_sep_(), ",")
})

# ---- Number formatting with locale ----

test_that("German locale formats numbers with dot thousands", {
  earthUI:::set_locale_("de")
  on.exit(withr_reset())
  result <- earthUI:::dollar_format_(c(1234567, 89012))
  expect_match(result[1], "^1\\.234\\.567$")
  expect_match(result[2], "^89\\.012$")
})

test_that("Finnish locale formats numbers with space thousands", {
  earthUI:::set_locale_("fi")
  on.exit(withr_reset())
  result <- earthUI:::dollar_format_(c(1234567, 89012))
  expect_match(result[1], "^1 234 567$")
})

test_that("Swiss locale formats numbers with apostrophe thousands", {
  earthUI:::set_locale_("ch")
  on.exit(withr_reset())
  result <- earthUI:::dollar_format_(c(1234567, 89012))
  expect_match(result[1], "^1'234'567$")
})

test_that("EU comma_format_ uses locale decimal mark", {
  earthUI:::set_locale_("de")
  on.exit(withr_reset())
  result <- earthUI:::comma_format_(c(1.1, 5.5))
  expect_match(result[1], "1,10$")
})

test_that("slope labels use locale formatting", {
  earthUI:::set_locale_("de")
  on.exit(withr_reset())
  labels <- earthUI:::format_slope_labels_(c(1234), c(200000, 500000))
  expect_match(labels[1], "^\\+1\\.234")
})

# ---- CSV import with locale ----

test_that("import_data reads semicolon-separated CSV with comma decimal", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c("price;area;name",
               "250000,50;120,5;Berlin",
               "300000,00;150,0;Munich"), tmp)
  df <- import_data(tmp, sep = ";", dec = ",")
  expect_equal(nrow(df), 2L)
  expect_equal(df$price[1], 250000.50)
  expect_equal(df$area[2], 150.0)
})

test_that("import_data with US defaults reads comma-separated CSV", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  write.csv(data.frame(price = c(100.5, 200.75)), tmp, row.names = FALSE)
  df <- import_data(tmp)
  expect_equal(df$price[1], 100.5)
})

# ---- Date format ordering ----

test_that("US locale tries MM/DD first", {
  earthUI:::set_locale_("us")
  fmts <- earthUI:::locale_date_formats_()
  expect_equal(fmts[1], "%m/%d/%Y")
})

test_that("German locale tries DD/MM first", {
  earthUI:::set_locale_("de")
  on.exit(withr_reset())
  fmts <- earthUI:::locale_date_formats_()
  expect_equal(fmts[1], "%d/%m/%Y")
})

test_that("Swedish locale tries YYYY-MM-DD first", {
  earthUI:::set_locale_("se")
  on.exit(withr_reset())
  fmts <- earthUI:::locale_date_formats_()
  expect_equal(fmts[1], "%Y-%m-%d")
})

test_that("all date format lists include ISO format", {
  for (code in c("us", "de", "fi", "jp", "ua")) {
    earthUI:::set_locale_(code)
    fmts <- earthUI:::locale_date_formats_()
    expect_true("%Y-%m-%d" %in% fmts, info = paste(code, "missing ISO format"))
  }
  withr_reset()
})

# ---- Paper size ----

test_that("US and Mexico use letter paper", {
  for (code in c("us", "mx")) {
    earthUI:::set_locale_(code)
    expect_equal(earthUI:::locale_paper_(), "letter", info = code)
  }
  withr_reset()
})

test_that("European countries use A4 paper", {
  for (code in c("de", "fr", "fi", "gb", "ua", "ru", "tr", "jp")) {
    earthUI:::set_locale_(code)
    expect_equal(earthUI:::locale_paper_(), "a4", info = code)
  }
  withr_reset()
})

# ---- PDF paper size injection ----

test_that("render_report accepts paper_size parameter", {
  # Just verify the function signature accepts it without error
  expect_true("paper_size" %in% names(formals(render_report)))
})

# ---- Locale persistence (SQLite) ----

test_that("locale defaults can be saved and loaded from SQLite", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  con <- earthUI:::settings_db_connect_()
  on.exit(earthUI:::settings_db_disconnect_(con))

  # Save locale defaults
  locale_json <- jsonlite::toJSON(list(
    locale_country = "fi",
    locale_paper = "a4",
    locale_csv_sep = ";",
    locale_dec = ",",
    locale_date = "dmy"
  ), auto_unbox = TRUE)
  earthUI:::settings_db_write_(con, "__locale_defaults__", settings = locale_json)

  # Read back
  saved <- earthUI:::settings_db_read_(con, "__locale_defaults__")
  expect_false(is.null(saved))
  expect_equal(saved$settings$locale_country, "fi")
  expect_equal(saved$settings$locale_paper, "a4")
  expect_equal(saved$settings$locale_csv_sep, ";")

  # Clean up
  DBI::dbExecute(con, "DELETE FROM file_settings WHERE filename = '__locale_defaults__'")
})
