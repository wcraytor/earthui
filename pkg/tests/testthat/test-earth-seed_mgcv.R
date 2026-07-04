# mgcvUI consumes an imported earth model differently from glmnetUI: it SEEDS
# the variable config (Include / Linear / Factor) and supplies spline knots,
# but the GAM is fit from the user's selections, so nothing is locked. These
# tests confirm earth pre-checks its predictors while leaving every control
# editable (no disabled inputs), and that the explanatory note is shown.

render_var_table <- function(earth = NULL, purpose = "general") {
  df <- data.frame(V1 = stats::rnorm(20), V2 = stats::rnorm(20),
                   V3 = stats::rnorm(20), y = stats::rnorm(20))
  html <- NULL
  shiny::testServer(
    mod_variables_server,
    args = list(data_r = shiny::reactive(df),
                earth_knots_r = shiny::reactive(earth),
                purpose_r = shiny::reactive(purpose)),
    {
      session$setInputs(response = "y")
      html <<- paste(as.character(output$var_table), collapse = "")
    }
  )
  html
}

test_that("earth import seeds Include/Linear for its predictors, fully editable", {
  ek <- list(predictors = c("V1", "V3"), linpreds = "V3",
             categoricals = character(0), knots = list())
  html <- render_var_table(ek)

  # Earth predictors: Include pre-checked (seeded)
  expect_match(html, "class='mgcv-inc' data-var='V1' checked")
  expect_match(html, "class='mgcv-inc' data-var='V3' checked")
  # Non-earth predictor: Include NOT seeded
  expect_false(grepl("class='mgcv-inc' data-var='V2' checked", html))
  # Linear seeded for the earth linear predictor
  expect_match(html, "class='mgcv-linear' data-var='V3' checked")

  # Crucially: nothing is locked. mgcvUI fits from the UI selections, so the
  # controls stay editable (no disabled attribute anywhere).
  expect_false(grepl("disabled", html))

  # The non-locking seed note is shown
  expect_match(html, "earth-seed-note")
})

test_that("without an earth import nothing is seeded and no note is shown", {
  html <- render_var_table(NULL)
  expect_false(grepl("class='mgcv-inc' data-var='V1' checked", html))
  expect_false(grepl("class='mgcv-inc' data-var='V3' checked", html))
  expect_false(grepl("earth-seed-note", html))
  expect_false(grepl("disabled", html))
})
