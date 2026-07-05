# glmnet module standalone: open the appraisal project, fit on raw
# variables (no earth import), verify results render.
scenario_glmnet_basic <- function(fx) {
  ok <- TRUE
  proc <- e2e_start_app("glmnet", fx$root)
  on.exit(e2e_stop_app(proc), add = TRUE)
  b <- e2e_session(PORTS[["glmnet"]])
  on.exit(try(b$close(), silent = TRUE), add = TRUE)

  e2e_pick_project(b, fx$appr)
  ok <- e2e_check("variable table rendered",
                  as.numeric(e2e_js(b,
                    "document.querySelectorAll('.glmnet-var-cb').length")) > 0) && ok

  e2e_js(b, "Shiny.setInputValue('data-response', 'sale_price');")
  e2e_include_vars(b, "glmnet-var-cb", c("gla", "lot", "age"))
  fit_btn <- e2e_js(b, "var b=document.getElementById('model-fit_btn'); b?b.id:''")
  ok <- e2e_check("fit button found", nzchar(fit_btn)) && ok
  if (nzchar(fit_btn)) {
    e2e_js(b, sprintf("document.getElementById('%s').click();", fit_btn))
    Sys.sleep(15)   # glmnet fits are fast; no shared modal protocol here
    n_out <- as.numeric(e2e_js(b,
      "document.querySelectorAll('.shiny-bound-output').length"))
    ok <- e2e_check("outputs bound after fit", n_out > 10) && ok
  }
  e2e_screenshot(b, "glmnet_after_fit.png")
  ok
}
