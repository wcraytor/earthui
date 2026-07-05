# Unit tests for the user-scripted derivation-rules engine.

test_that("apply_user_rules_ derives counts, merges and arithmetic columns", {
  testthat::skip_if_not(vProlog_available(), "vProlog engine not installed")
  # The embedded Trealla engine builds on Windows but does not yet answer
  # queries there (tracked; needs debugging on a real Windows machine).
  # Prolog rules features are documented as not yet supported on Windows.
  testthat::skip_on_os("windows")

  df <- data.frame(
    pr_has_solar        = c(TRUE,  FALSE, TRUE),
    pr_solar_owned      = c(TRUE,  FALSE, FALSE),
    pr_has_battery      = c(FALSE, FALSE, TRUE),
    pr_has_pool         = c(TRUE,  FALSE, FALSE),
    pr_condition_score  = c(4,     2,     5),
    pr_outbuilding_sqft = c(0,     0,     1200)
  )
  rules <- paste(readLines(test_path("example_rules.pl")), collapse = "\n")
  out <- apply_user_rules_(df, rules)

  # boolean OR-merge
  expect_equal(out$solar_any, c(TRUE, FALSE, TRUE))
  # counts (numeric)
  expect_equal(out$solar_count,   c(2, 0, 2))   # of {solar, owned, battery}
  expect_equal(out$amenity_count, c(2, 0, 2))   # of pr_has_* : solar+pool / - / solar+battery
  # threshold + fallback
  expect_equal(out$big_outbuilding, c(FALSE, FALSE, TRUE))
  # arithmetic -> continuous real column
  expect_equal(out$cond_scaled, c(6, 3, 7.5))

  # types: counts numeric, merge logical
  expect_type(out$solar_count, "double")
  expect_type(out$solar_any,   "logical")
})

test_that("default_rules_template_ ships only commented examples", {
  tpl   <- default_rules_template_()
  expect_true(grepl("derive\\(", tpl))           # contains example rules
  lines <- strsplit(tpl, "\n", fixed = TRUE)[[1]]
  nonblank <- lines[nzchar(trimws(lines))]
  # every non-blank line is a comment -> an untouched template derives nothing
  expect_true(all(grepl("^\\s*%", nonblank)))
})
