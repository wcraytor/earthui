# Per-purpose parametrized tests.
#
# The three purpose modes (general / appraisal / market) share one modeling
# engine but diverge on a few isolated forks. The most important is first-row
# handling: appraisal ALWAYS skips row 1, while general/market skip it only when
# the user ticks the checkbox. Testing appraisal alone never exercises the
# "keep all rows" / "optional skip" branch that general and market rely on, so
# we drive a real modelingServer fit with skip_first_row_r toggled and assert
# the fitted design matrix has the expected number of rows.

# Build data_module mocks for a clean N-row numeric dataset.
make_data_module <- function(n = 14L) {
  set.seed(11)
  V1 <- stats::rnorm(n); V2 <- stats::rnorm(n); V3 <- stats::rnorm(n)
  y  <- 2 * V1 - 1.5 * V2 + 0.5 * V3 + stats::rnorm(n, sd = 0.3)
  dat <- data.frame(V1 = V1, V2 = V2, V3 = V3, y = y)
  empty_named <- stats::setNames(character(0), character(0))
  list(
    data           = shiny::reactiveVal(dat),
    response       = shiny::reactiveVal("y"),
    predictors     = shiny::reactiveVal(c("V1", "V2", "V3")),
    force_in       = shiny::reactiveVal(character(0)),
    expected_signs = shiny::reactiveVal(empty_named),
    col_types      = shiny::reactiveVal(c(V1 = "numeric", V2 = "numeric",
                                          V3 = "numeric")),
    fac_vars       = shiny::reactiveVal(character(0)),
    file_name      = shiny::reactiveVal("test.csv"),
    col_specials   = shiny::reactiveVal(empty_named),
    weight_col     = shiny::reactiveVal(NULL),
    valid          = shiny::reactiveVal(TRUE),
    rv             = shiny::reactiveValues(data = dat)
  )
}

# Drive a single CV fit and return nrow(x_matrix), or NULL if the fit failed.
fit_nrows <- function(purpose, skip, n = 14L) {
  dm <- make_data_module(n)
  out <- NULL
  shiny::testServer(
    modelingServer,
    args = list(data_module = dm,
                purpose = shiny::reactiveVal(purpose),
                skip_first_row_r = shiny::reactiveVal(skip)),
    {
      session$setInputs(
        alpha_method = "single", alpha = 1,
        lambda_method = "cv", lambda_choice = "1se",
        nfolds = 5, type_measure = "mse",
        family = "gaussian", standardize = TRUE, relaxed = FALSE,
        enforce_signs = FALSE, random_seed = 42L
      )
      session$setInputs(fit_btn = 1)
      if (isTRUE(session$returned$fitted())) {
        out <<- nrow(session$returned$x_matrix())
      }
    }
  )
  out
}

test_that("general mode keeps all rows when skip is off (the non-appraisal branch)", {
  expect_equal(fit_nrows("general", skip = FALSE, n = 30L), 30L)
})

test_that("general mode drops row 1 when the skip checkbox is on", {
  expect_equal(fit_nrows("general", skip = TRUE, n = 30L), 29L)
})

test_that("market mode keeps all rows when skip is off", {
  expect_equal(fit_nrows("market", skip = FALSE, n = 30L), 30L)
})

test_that("market mode drops row 1 when skip is on", {
  expect_equal(fit_nrows("market", skip = TRUE, n = 30L), 29L)
})

test_that("appraisal mode drops row 1 (skip forced on by the server)", {
  # In the live app the server forces skip_first_row = TRUE for appraisal; here
  # we pass skip = TRUE to mirror that, confirming the subject row is excluded.
  expect_equal(fit_nrows("appraisal", skip = TRUE, n = 30L), 29L)
})

test_that("summaryServer returns summary_stats for every purpose mode", {
  for (p in c("general", "appraisal", "market")) {
    set.seed(7); n <- 60
    x <- matrix(stats::rnorm(n * 3), n, 3,
                dimnames = list(NULL, c("V1", "V2", "V3")))
    yv <- x[, 1] * 2 - x[, 2] + stats::rnorm(n)
    fit <- glmnet::cv.glmnet(x, yv)
    model_module <- list(
      model = shiny::reactiveVal(fit),
      lambda = shiny::reactiveVal(fit$lambda.1se),
      gamma = shiny::reactiveVal(NULL),
      x_matrix = shiny::reactiveVal(x),
      y_vector = shiny::reactiveVal(yv),
      family = shiny::reactiveVal("gaussian"),
      fitted = shiny::reactiveVal(TRUE),
      fit_count = shiny::reactiveVal(1L)
    )
    data_module <- list(
      expected_signs = shiny::reactiveVal(c(V1 = "positive", V2 = "negative"))
    )
    shiny::testServer(
      summaryServer,
      args = list(model_module = model_module, data_module = data_module,
                  purpose = shiny::reactiveVal(p)),
      {
        stats <- session$returned$summary_stats()
        expect_false(is.null(stats),
                     info = paste("summary_stats NULL for purpose", p))
      }
    )
  }
})
