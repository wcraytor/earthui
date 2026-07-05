# Shared helpers for the earthUI end-to-end browser smoke suite.
# Requirements: chromote (+ Chrome/Chromium), callr, writexl, jsonlite, and an
# INSTALLED earthUI (the suite drives the installed package, like a user).
# This suite is a DEVELOPER tool (run before releases); it is not part of the
# CRAN test suite and never runs on CRAN.

suppressMessages({
  library(chromote)
})

PORTS <- c(earth = 7891L, glmnet = 7892L, mgcv = 7893L)

# ---- fixtures ---------------------------------------------------------------

# Build a throwaway REGPROJ_ROOT with one appraisal project (xlsx with logical
# pr_* columns + a subject row) and one general project (csv), plus an earth
# result RDS in the appraisal project's mac_out_earth for import tests.
e2e_fixture_root <- function() {
  root <- file.path(tempdir(), paste0("e2e_root_", as.integer(Sys.time())))
  old <- Sys.getenv("REGPROJ_ROOT", unset = NA)
  Sys.setenv(REGPROJ_ROOT = root)
  on.exit(if (is.na(old)) Sys.unsetenv("REGPROJ_ROOT")
          else Sys.setenv(REGPROJ_ROOT = old), add = TRUE)

  set.seed(42)
  mk_df <- function(n, subject_row = FALSE) {
    df <- data.frame(
      sale_price = round(rlnorm(n, 13, .3)),
      gla  = round(rnorm(n, 1600, 300)),
      lot  = round(rnorm(n, 6000, 1500)),
      age  = sample(0:70, n, TRUE),
      pr_pool = sample(c(TRUE, FALSE), n, TRUE),
      pr_view = sample(c(TRUE, FALSE), n, TRUE),
      qual = sample(c("good", "avg", "fair"), n, TRUE))
    if (subject_row) df$sale_price[1] <- NA
    df
  }

  # appraisal project (xlsx)
  in_a <- earthUI::regproj_path("appr", "us", c("ca", "081", "e2ecty"), "E2Eappr",
                                os = "mac", in_or_out = "in", create = TRUE)
  for (m in c("earth", "glmnet", "mgcv"))
    earthUI::regproj_path("appr", "us", c("ca", "081", "e2ecty"), "E2Eappr",
                          os = "mac", in_or_out = "out", method = m,
                          create = TRUE)
  writexl::write_xlsx(mk_df(200, subject_row = TRUE),
                      file.path(in_a, "e2e_appr.xlsx"))
  proj_a <- dirname(in_a)
  earthUI::register_project(proj_a, "appr", "us", c("ca", "081", "e2ecty"),
                            "E2Eappr", last_file = "e2e_appr.xlsx", root = root)
  earthUI::regproj_last_file_set(proj_a, "e2e_appr.xlsx")

  # general project (csv)
  in_g <- earthUI::regproj_path("gen", "us", c("ca", "081", "e2ecty"), "E2Egen",
                                os = "mac", in_or_out = "in", create = TRUE)
  utils::write.csv(mk_df(200), file.path(in_g, "e2e_gen.csv"),
                   row.names = FALSE)
  proj_g <- dirname(in_g)
  earthUI::register_project(proj_g, "gen", "us", c("ca", "081", "e2ecty"),
                            "E2Egen", last_file = "e2e_gen.csv", root = root)
  earthUI::regproj_last_file_set(proj_g, "e2e_gen.csv")

  # earth result RDS in the appraisal project (for module import tests),
  # named like the app's auto-export
  df <- as.data.frame(readxl::read_excel(file.path(in_a, "e2e_appr.xlsx")))
  res <- earthUI::fit_earth(df[-1, ], target = "sale_price",
                            predictors = c("gla", "lot", "age", "qual"),
                            categoricals = "qual", degree = 1L)
  rds <- file.path(proj_a, "mac_out_earth",
                   "e2e_appr_earthUI_result_20260704_090000.rds")
  saveRDS(res, rds)

  list(root = root, appr = proj_a, gen = proj_g, rds = rds)
}

# ---- app lifecycle ----------------------------------------------------------

e2e_port_open <- function(port) {
  con <- suppressWarnings(tryCatch(
    socketConnection("127.0.0.1", port = port, timeout = 1, open = "r+"),
    error = function(e) NULL))
  if (is.null(con)) return(FALSE)
  close(con); TRUE
}

# Start one of the three apps against the fixture root; returns the process.
e2e_start_app <- function(which, root, port = PORTS[[which]]) {
  stopifnot(which %in% names(PORTS))
  proc <- callr::r_bg(
    function(which, port) {
      fn <- switch(which, earth = earthUI::launch,
                   glmnet = earthUI::launch_glmnet,
                   mgcv = earthUI::launch_mgcv)
      fn(port = port, launch.browser = FALSE)
    },
    args = list(which = which, port = port),
    env = c(callr::rcmd_safe_env(), REGPROJ_ROOT = root),
    supervise = TRUE, stdout = "|", stderr = "|")
  for (i in 1:60) {
    if (e2e_port_open(port)) return(proc)
    if (!proc$is_alive())
      stop(which, " app died on startup:\n",
           paste(proc$read_all_error_lines(), collapse = "\n"))
    Sys.sleep(0.5)
  }
  stop(which, " app did not serve within 30s")
}

e2e_stop_app <- function(proc) {
  if (!is.null(proc) && proc$is_alive()) try(proc$kill(), silent = TRUE)
  invisible(NULL)
}

# ---- browser driving --------------------------------------------------------

e2e_session <- function(port, width = 1400, height = 950) {
  b <- ChromoteSession$new(width = width, height = height)
  b$Page$navigate(sprintf("http://127.0.0.1:%d", port))
  Sys.sleep(6)
  b
}

e2e_js <- function(b, code) {
  r <- b$Runtime$evaluate(code, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails))
    stop("JS error: ", r$exceptionDetails$exception$description)
  r$result$value
}

e2e_pick_project <- function(b, project_path, settle = 8) {
  e2e_js(b, sprintf("Shiny.setInputValue('regproj_project_pick', '%s');",
                    project_path))
  Sys.sleep(settle)
}

# mgcv/glmnet module tables use .mgcv-inc / .glmnet-inc checkboxes with
# data-var attributes; earth's monolith uses its own ids. Scenario scripts
# handle app-specific driving; these helpers cover the module apps.
e2e_include_vars <- function(b, cls, vars) {
  e2e_js(b, sprintf(
    "%s.forEach(function(v){var el=document.querySelector('.%s[data-var=\"'+v+'\"]'); if(el && !el.checked) el.click();});",
    jsonlite::toJSON(vars), cls))
  Sys.sleep(1)
}

# Wait for a fitting modal driven by the shared 'fitting_done' protocol.
e2e_wait_fit <- function(b, log_id, timeout_s = 180) {
  for (i in seq_len(ceiling(timeout_s / 2))) {
    Sys.sleep(2)
    t <- e2e_js(b, sprintf(
      "var el=document.getElementById('%s'); el?el.textContent.slice(-160):''",
      log_id))
    if (grepl("Nothing else is coming|Error", t)) return(t)
  }
  stop("fit did not complete within ", timeout_s, "s")
}

e2e_click_tab <- function(b, tabset_id, label, settle = 4) {
  e2e_js(b, sprintf(
    "var tabs=document.querySelectorAll('#%s a'); tabs.forEach(function(a){if(a.textContent.trim()==='%s') a.click();});",
    tabset_id, label))
  Sys.sleep(settle)
}

e2e_content_len <- function(b, id) {
  as.numeric(e2e_js(b, sprintf(
    "var e=document.getElementById('%s'); e?e.innerHTML.length:-1", id)))
}

e2e_screenshot <- function(b, name) {
  dir.create("tools/e2e/screenshots", showWarnings = FALSE, recursive = TRUE)
  f <- file.path("tools/e2e/screenshots", name)
  b$screenshot(f)
  f
}

# ---- reporting --------------------------------------------------------------

e2e_check <- function(label, ok, detail = "") {
  status <- if (isTRUE(ok)) "PASS" else "FAIL"
  cat(sprintf("  [%s] %s%s\n", status, label,
              if (nzchar(detail)) paste0(" — ", detail) else ""))
  isTRUE(ok)
}
