# When an earthUI model is imported, the predictor set is dictated by earth's
# basis, so the Variable Configuration table locks Include + Type, disables
# rows for predictors earth did not use, and keeps Force/Sign editable for
# earth's predictors. These tests render the variable_table via testServer and
# assert the resulting HTML carries the right disabled/checked attributes.

# Render dataImportServer's variable_table with a given earth import and return
# the HTML string. The double flush lets the active-project observer (which
# clears rv$data on init) fire before we seed the data manually.
render_var_table <- function(earth_import = NULL) {
  dat <- data.frame(V1 = stats::rnorm(20), V2 = stats::rnorm(20),
                    V3 = stats::rnorm(20), y = stats::rnorm(20))
  html <- NULL
  shiny::testServer(
    dataImportServer,
    args = list(purpose = shiny::reactiveVal("general"),
                earth_import_r = shiny::reactiveVal(earth_import)),
    {
      session$setInputs(response = "y")
      session$flushReact()
      rv$data <- dat
      rv$col_types <- c(V1 = "numeric", V2 = "numeric",
                        V3 = "numeric", y = "numeric")
      rv$file_name <- "t.csv"
      session$flushReact()
      html <<- paste(as.character(output$variable_table), collapse = "")
    }
  )
  html
}

# Extract the <input> tag for a given control id substring.
input_tag <- function(html, id_frag) {
  regmatches(html, regexpr(paste0("<input[^>]*", id_frag, "[^>]*>"), html))
}

test_that("earth import locks Include + Type and reflects earth's predictor set", {
  ek <- structure(list(predictors = c("V1", "V3")),
                  class = "glmnetUI_earth_import")
  html <- render_var_table(ek)

  # Earth predictors: Include checked AND disabled
  inc_v1 <- input_tag(html, "inc_V1")
  inc_v3 <- input_tag(html, "inc_V3")
  expect_match(inc_v1, "disabled"); expect_match(inc_v1, "checked")
  expect_match(inc_v3, "disabled"); expect_match(inc_v3, "checked")

  # Non-earth predictor: Include disabled but NOT checked
  inc_v2 <- input_tag(html, "inc_V2")
  expect_match(inc_v2, "disabled")
  expect_false(grepl("checked", inc_v2))

  # Type is locked for everyone
  expect_true(grepl("type_V1[^>]*disabled", html))
  expect_true(grepl("type_V2[^>]*disabled", html))

  # Force stays editable for earth predictors, disabled for non-earth
  expect_false(grepl("force_V1[^>]*disabled", html))
  expect_false(grepl("force_V3[^>]*disabled", html))
  expect_true(grepl("force_V2[^>]*disabled", html))

  # Explanatory banner is shown
  expect_match(html, "earth-lock-note")
})

test_that("without an earth import the table is fully editable (no lock)", {
  html <- render_var_table(NULL)
  # No earth -> no disabled inputs and no banner
  expect_false(grepl("inc_V1[^>]*disabled", html))
  expect_false(grepl("type_V1[^>]*disabled", html))
  expect_false(grepl("force_V1[^>]*disabled", html))
  expect_false(grepl("earth-lock-note", html))
})
