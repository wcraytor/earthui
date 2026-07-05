# earth app: open the appraisal project, verify the Post Processing section,
# click Open glmnet, verify the module serves and auto-opens the project,
# then build the Combined Conclusion (registrations exist from prior fits).
scenario_earth_support <- function(fx) {
  ok <- TRUE
  proc <- e2e_start_app("earth", fx$root)
  on.exit(e2e_stop_app(proc), add = TRUE)
  b <- e2e_session(PORTS[["earth"]])
  on.exit(try(b$close(), silent = TRUE), add = TRUE)

  e2e_pick_project(b, fx$appr)
  ok <- e2e_check("Post Processing section present",
                  e2e_js(b, "!!document.getElementById('support_glmnet_btn')")) && ok

  e2e_js(b, "var d=document.getElementById('support_glmnet_btn').closest('details'); if(d) d.open = true;")
  e2e_js(b, "document.getElementById('support_glmnet_btn').click();")
  up <- FALSE
  for (i in 1:40) { Sys.sleep(1)
    st <- e2e_js(b, "var e=document.getElementById('support_fit_status'); e?e.textContent:''")
    if (grepl("running", st)) { up <- TRUE; break }
  }
  ok <- e2e_check("glmnet module spawned and serving", up) && ok
  if (up) {
    b2 <- ChromoteSession$new()
    b2$Page$navigate("http://127.0.0.1:7879"); Sys.sleep(7)
    ok <- e2e_check("module auto-opened the project",
                    isTRUE(b2$Runtime$evaluate(
                      "document.body.textContent.indexOf('E2Eappr') >= 0",
                      returnByValue = TRUE)$result$value)) && ok
    try(b2$close(), silent = TRUE)
  }

  # Combined Conclusion: fits were registered by scenarios 1-2's runs
  e2e_js(b, "document.getElementById('combined_report_btn').click();")
  Sys.sleep(4)
  combined_dir <- file.path(fx$appr, "trilogy", "mac_out", "combined")
  reports <- list.files(combined_dir, pattern = "^Trilogy_Report_.*html$",
                        full.names = TRUE)
  ok <- e2e_check("combined report written", length(reports) >= 1) && ok
  if (length(reports)) {
    html <- paste(readLines(reports[1], warn = FALSE), collapse = "")
    ok <- e2e_check("earth marked as the value conclusion",
                    grepl("value conclusion", html)) && ok
    ok <- e2e_check("modules labeled Post Processing",
                    grepl("Post Processing Module", html)) && ok
  }
  e2e_screenshot(b, "earth_support_section.png")
  ok
}
