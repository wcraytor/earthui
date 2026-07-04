# --- Module UI functions return shiny tags ---

test_that("mod_data_ui returns a shiny tag list", {
  ui <- mod_data_ui("test1")
  expect_true(inherits(ui, c("shiny.tag", "shiny.tag.list")))
})

test_that("mod_data_ui contains namespaced id", {
  ui <- as.character(mod_data_ui("test1"))
  expect_true(grepl("test1", ui))
})

test_that("mod_earth_import_ui returns a shiny tag list", {
  ui <- mod_earth_import_ui("test1")
  expect_true(inherits(ui, c("shiny.tag", "shiny.tag.list")))
})

test_that("mod_earth_import_ui contains namespaced id", {
  ui <- as.character(mod_earth_import_ui("test1"))
  expect_true(grepl("test1", ui))
})

test_that("mod_model_fit_ui returns a shiny tag list", {
  ui <- mod_model_fit_ui("test1")
  expect_true(inherits(ui, c("shiny.tag", "shiny.tag.list")))
})

test_that("mod_model_fit_ui contains namespaced id", {
  ui <- as.character(mod_model_fit_ui("test1"))
  expect_true(grepl("test1", ui))
})

test_that("mod_model_results_ui returns a shiny tag", {
 ui <- mod_model_results_ui("test1")
  expect_true(inherits(ui, c("shiny.tag", "shiny.tag.list")))
})

test_that("mod_model_results_ui contains namespaced id", {
  ui <- as.character(mod_model_results_ui("test1"))
  expect_true(grepl("test1", ui))
})

test_that("mod_report_ui returns a shiny tag list", {
  ui <- mod_report_ui("test1")
  expect_true(inherits(ui, c("shiny.tag", "shiny.tag.list")))
})

test_that("mod_report_ui contains namespaced id", {
  ui <- as.character(mod_report_ui("test1"))
  expect_true(grepl("test1", ui))
})

test_that("mod_variables_ui returns a shiny tag list", {
  ui <- mod_variables_ui("test1")
  expect_true(inherits(ui, c("shiny.tag", "shiny.tag.list")))
})

test_that("mod_variables_ui contains namespaced id", {
  ui <- as.character(mod_variables_ui("test1"))
  expect_true(grepl("test1", ui))
})

test_that("mod_variables_params_ui returns a shiny tag list", {
  ui <- mod_variables_params_ui("test1")
  expect_true(inherits(ui, c("shiny.tag", "shiny.tag.list")))
})

test_that("mod_variables_params_ui contains namespaced id", {
  ui <- as.character(mod_variables_params_ui("test1"))
  expect_true(grepl("test1", ui))
})

# --- Server functions exist and have correct formals ---

test_that("mod_data_server is a function with correct formals", {
  expect_true(is.function(mod_data_server))
  fmls <- names(formals(mod_data_server))
  expect_true("id" %in% fmls)
  expect_true("active_project_r" %in% fmls)
})

test_that("mod_earth_import_server is a function with correct formals", {
  expect_true(is.function(mod_earth_import_server))
  fmls <- names(formals(mod_earth_import_server))
  expect_equal(fmls, "id")
})

test_that("mod_model_server is a function with correct formals", {
  expect_true(is.function(mod_model_server))
  fmls <- names(formals(mod_model_server))
  expect_true("id" %in% fmls)
  expect_true("data_r" %in% fmls)
  expect_true("var_config_r" %in% fmls)
  expect_true("earth_knots_r" %in% fmls)
  expect_true("dark_mode_r" %in% fmls)
})

test_that("mod_report_server is a function with correct formals", {
  expect_true(is.function(mod_report_server))
  fmls <- names(formals(mod_report_server))
  expect_true("id" %in% fmls)
  expect_true("gam_result_r" %in% fmls)
  expect_true("data_r" %in% fmls)
})

test_that("mod_variables_server is a function with correct formals", {
  expect_true(is.function(mod_variables_server))
  fmls <- names(formals(mod_variables_server))
  expect_true("id" %in% fmls)
  expect_true("data_r" %in% fmls)
  expect_true("filename_r" %in% fmls)
  expect_true("earth_knots_r" %in% fmls)
  expect_true("purpose_r" %in% fmls)
})

# --- UI output includes expected widgets ---

test_that("mod_data_ui includes the project file picker", {
  ui <- as.character(mod_data_ui("d"))
  expect_true(grepl("d-file_picker", ui))
  expect_true(grepl("d-files_refresh", ui))
})

test_that("mod_model_fit_ui includes fit button", {
  ui <- as.character(mod_model_fit_ui("m"))
  expect_true(grepl("m-fit", ui))
  expect_true(grepl("Fit Model", ui))
})

test_that("mod_report_ui includes download controls", {
  ui <- as.character(mod_report_ui("r"))
  expect_true(grepl("r-download", ui))
  expect_true(grepl("r-format", ui))
})

test_that("mod_variables_ui includes response selector", {
  ui <- as.character(mod_variables_ui("v"))
  expect_true(grepl("v-response", ui))
})

test_that("mod_earth_import_ui includes browse button", {
  ui <- as.character(mod_earth_import_ui("e"))
  expect_true(grepl("e-earth_browse", ui))
})
