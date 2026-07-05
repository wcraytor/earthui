# earthUI end-to-end browser smoke suite

Drives the **installed** earthUI package in a real headless Chrome, the way a
user would: launches each app, opens fixture regProj projects, fits models,
and asserts that results actually reach the browser.

    Rscript tools/e2e/run_all.R

Requirements: an installed earthUI, `chromote` (+ Chrome/Chromium), `callr`,
`writexl`, `readxl`, `jsonlite`. Runtime is a few minutes. Screenshots land in
`tools/e2e/screenshots/` (gitignored) for eyeballing.

This is a **developer tool**, run before releases. It is deliberately not part
of `pkg/tests/` — CRAN machines have no browser, and these tests exercise the
installed package rather than the source tree.

Why it exists: the unit suite (2,500+ assertions) cannot see browser-level
failures. This rig's ancestor caught a post-merge bug where a GAM fit
completed ("Done") but a session-killing error during the reactive flush left
every results tab blank — invisible to unit tests, obvious to a robot that
clicks Fit and measures the Summary tab's innerHTML.

Layout: `helpers.R` (fixtures, app lifecycle, browser driving, assertions),
`scenarios/*.R` (one flow per file, each a `scenario_*()` returning TRUE on
pass), `run_all.R` (runner; exit code 1 on any failure).
