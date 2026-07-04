# helper-settings_db.R — Redirect settings DB to a temp file during tests
# This file is automatically sourced by testthat before any test files run.

local_test_settings_db <- function(env = parent.frame()) {
  tmp <- tempfile(fileext = ".sqlite")
  old <- getOption("mgcvUI.settings_db_path")
  options(mgcvUI.settings_db_path = tmp)
  withr::defer({
    options(mgcvUI.settings_db_path = old)
    unlink(tmp, force = TRUE)
  }, envir = env)
  invisible(tmp)
}
