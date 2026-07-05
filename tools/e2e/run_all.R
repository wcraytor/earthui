#!/usr/bin/env Rscript
# earthUI end-to-end browser smoke suite.
#   Rscript tools/e2e/run_all.R          (from the repo root)
# Drives the INSTALLED earthUI package in a real headless Chrome via
# chromote. Developer tool — run before releases; never on CRAN.

setwd(normalizePath(file.path(dirname(
  sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))),
  "..", "..")))
source("tools/e2e/helpers.R")
for (f in sort(list.files("tools/e2e/scenarios", full.names = TRUE)))
  source(f)

cat("== earthUI E2E smoke suite ==\n")
cat("building fixtures...\n")
fx <- e2e_fixture_root()

scenarios <- list(
  "mgcv: project + earth import + fit + tabs" = scenario_mgcv_basic,
  "glmnet: project + fit"                     = scenario_glmnet_basic,
  "earth: Post Processing + Combined"         = scenario_earth_support)

results <- logical(0)
for (nm in names(scenarios)) {
  cat("\n--", nm, "--\n")
  results[[nm]] <- tryCatch(isTRUE(scenarios[[nm]](fx)),
                            error = function(e) {
                              cat("  [FAIL]", conditionMessage(e), "\n")
                              FALSE
                            })
}

cat("\n== summary ==\n")
for (nm in names(results))
  cat(sprintf("  %s  %s\n", if (results[[nm]]) "PASS" else "FAIL", nm))
if (!all(unlist(results))) quit(status = 1)
cat("all scenarios passed\n")
