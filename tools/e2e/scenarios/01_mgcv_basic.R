# mgcv module standalone: open the appraisal project (auto-loads its xlsx),
# import the earth result, fit, verify the Summary tab populates.
scenario_mgcv_basic <- function(fx) {
  ok <- TRUE
  proc <- e2e_start_app("mgcv", fx$root)
  on.exit(e2e_stop_app(proc), add = TRUE)
  b <- e2e_session(PORTS[["mgcv"]])
  on.exit(try(b$close(), silent = TRUE), add = TRUE)

  e2e_pick_project(b, fx$appr)
  ok <- e2e_check("variables render after project auto-load",
                  as.numeric(e2e_js(b,
                    "document.querySelectorAll('.mgcv-var-row').length")) > 0) && ok

  # earth import through the real shinyFiles input
  segs <- jsonlite::toJSON(as.list(strsplit(fx$rds, "/")[[1]]),
                           auto_unbox = TRUE)
  e2e_js(b, sprintf(
    "Shiny.setInputValue('earth-earth_browse', {files: {\"0\": %s}, root: \"Root\"}, {priority: 'event'});",
    segs))
  Sys.sleep(5)
  ok <- e2e_check("earth import summary shown",
                  grepl("Target", e2e_js(b,
                    "var e=document.getElementById('earth-earth_summary'); e?e.textContent:''"))) && ok

  e2e_js(b, "Shiny.setInputValue('vars-response', 'sale_price');")
  Sys.sleep(2)
  e2e_js(b, "document.getElementById('model-fit').click();")
  t <- e2e_wait_fit(b, "mgcv-trace-log")
  ok <- e2e_check("fit completed without error", !grepl("Error", t)) && ok

  e2e_click_tab(b, "model-results_tabs", "Summary")
  ok <- e2e_check("Summary tab populated",
                  e2e_content_len(b, "model-summary_metrics") > 100) && ok
  ok <- e2e_check("post-fit sidebar steps visible",
                  e2e_js(b,
                    "var d=document.querySelectorAll('[data-display-if=\"output.model_fitted\"]'); d.length ? d[0].style.display !== 'none' : false")) && ok
  e2e_screenshot(b, "mgcv_summary.png")
  ok
}
