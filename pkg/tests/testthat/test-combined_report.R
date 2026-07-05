# --- build_combined_report (comparative one-page summary) --------------------

test_that("build_combined_report assembles the comparative summary", {
  skip_if_not_installed("jsonlite")
  root <- tempfile("combined_")
  proj <- file.path(root, "appr", "us_ca_081_testcy_Comb")
  dir.create(proj, recursive = TRUE)

  # Register fits for two methods and drop a conclusion + grid for earth
  trilogy_register_fit(proj, "earth",  "20260704_120000")
  trilogy_register_fit(proj, "glmnet", "20260704_121500")
  outdir <- file.path(proj, "mac_out_earth")
  dir.create(outdir, recursive = TRUE)
  writeLines("x", file.path(outdir, "SalesGrid_20260704_120000.xlsx"))
  jsonlite::write_json(
    list(subject_value = 512345,
         metrics = list(r2 = 0.87, rmse = 21000)),
    file.path(outdir, "conclusion_20260704_120000.json"),
    auto_unbox = TRUE)

  out <- build_combined_report(proj, os = "mac")
  expect_true(file.exists(out))
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(html, "Combined Appraisal Report")
  expect_match(html, "512,345")                       # earth conclusion value
  expect_match(html, "value conclusion")              # earth marked primary
  expect_match(html, "Post Processing Module")        # modules labeled
  expect_match(html, "not run")                       # mgcv absent -> marked
  expect_match(html, "Sales Grid")                    # grid linked
  # written into the trilogy combined output folder
  expect_true(grepl(file.path("trilogy", "mac_out", "combined"), out,
                    fixed = TRUE))
})
