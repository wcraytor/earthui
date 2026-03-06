# Tests for render_report() error paths

test_that("render_report rejects non-earthUI_result", {
  expect_error(render_report(list()), "earthUI_result")
})

test_that("render_report rejects invalid output_format", {
  result <- fit_earth(mtcars, "mpg", c("wt", "hp"))
  expect_error(render_report(result, output_format = "txt"),
               "'arg' should be one of")
})

test_that("render_report errors when quarto package missing", {
  result <- fit_earth(mtcars, "mpg", c("wt", "hp"))

  # If quarto is not installed, we'll get the expected error
  if (!requireNamespace("quarto", quietly = TRUE)) {
    expect_error(render_report(result, output_format = "html"),
                 "quarto.*package is required")
  }
})

test_that("render_report accepts valid output_format values", {
  # Just test that match.arg doesn't reject these
  result <- fit_earth(mtcars, "mpg", c("wt", "hp"))
  for (fmt in c("html", "pdf", "docx")) {
    # If quarto is missing, we expect the quarto error, not a format error
    err <- tryCatch(render_report(result, output_format = fmt),
                    error = function(e) conditionMessage(e))
    if (is.character(err)) {
      expect_false(grepl("'arg' should be one of", err),
                   info = paste("Format", fmt, "should be accepted"))
    }
  }
})
