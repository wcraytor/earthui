# Shiny module server tests via shiny::testServer(). Each result module takes
# `model_module` / `data_module` lists of reactives; we build mock versions
# from a small fitted cv.glmnet model and assert on the returned values and
# internal reactives (the test block runs in the server's environment, so
# unexported reactives are reachable by name).

# Build a fitted model + matching data mocks shared across the tests below.
make_mocks <- function() {
  set.seed(7)
  n <- 150
  x <- matrix(stats::rnorm(n * 3), n, 3,
              dimnames = list(NULL, c("V1", "V2", "V3")))
  y <- x[, 1] * 2 + x[, 2] * (-1.5) + 0.5 * x[, 3] + stats::rnorm(n)
  fit <- glmnet::cv.glmnet(x, y)
  dat <- as.data.frame(x)
  dat$y <- y

  model_module <- list(
    model       = shiny::reactiveVal(fit),
    lambda      = shiny::reactiveVal(fit$lambda.1se),
    gamma       = shiny::reactiveVal(NULL),
    x_matrix    = shiny::reactiveVal(x),
    y_vector    = shiny::reactiveVal(y),
    family      = shiny::reactiveVal("gaussian"),
    earth_import = shiny::reactiveVal(NULL),
    fitted      = shiny::reactiveVal(TRUE),
    fit_count   = shiny::reactiveVal(1L)
  )
  data_module <- list(
    predictors     = shiny::reactiveVal(c("V1", "V2", "V3")),
    response       = shiny::reactiveVal("y"),
    data           = shiny::reactiveVal(dat),
    col_types      = shiny::reactiveVal(c(V1 = "numeric", V2 = "numeric",
                                          V3 = "numeric")),
    fac_vars       = shiny::reactiveVal(character(0)),
    expected_signs = shiny::reactiveVal(c(V1 = "positive", V2 = "negative"))
  )
  list(model_module = model_module, data_module = data_module)
}

test_that("summaryServer returns inspectable summary_stats", {
  m <- make_mocks()
  shiny::testServer(
    summaryServer,
    args = list(model_module = m$model_module, data_module = m$data_module,
                purpose = shiny::reactiveVal("general")),
    {
      stats <- session$returned$summary_stats()
      expect_false(is.null(stats))
    }
  )
})

test_that("equationServer returns a LaTeX equation string", {
  m <- make_mocks()
  shiny::testServer(
    equationServer,
    args = list(model_module = m$model_module, data_module = m$data_module),
    {
      eq <- session$returned$equation()
      # equation() returns a list of rendered forms: latex / latex_inline /
      # latex_pdf, each a non-empty LaTeX string.
      expect_type(eq, "list")
      expect_true(all(c("latex", "latex_inline", "latex_pdf") %in% names(eq)))
      expect_true(nzchar(eq$latex))
      expect_match(eq$latex, "V1")  # predictor appears in the equation
    }
  )
})

test_that("importanceServer computes an importance data frame", {
  m <- make_mocks()
  shiny::testServer(
    importanceServer,
    args = list(model_module = m$model_module, data_module = m$data_module),
    {
      df <- importance_df()
      expect_true(is.data.frame(df))
      expect_gt(nrow(df), 0L)
    }
  )
})

test_that("contributionsServer computes contribution data", {
  m <- make_mocks()
  shiny::testServer(
    contributionsServer,
    args = list(model_module = m$model_module, data_module = m$data_module),
    {
      cd <- contrib_data()
      expect_false(is.null(cd))
    }
  )
})

test_that("diagnosticsServer plot reactives build ggplot objects", {
  m <- make_mocks()
  shiny::testServer(
    diagnosticsServer,
    args = list(model_module = m$model_module),
    {
      expect_s3_class(coef_path_gg(), "ggplot")
      expect_s3_class(cv_gg(),        "ggplot")
      expect_s3_class(avp_gg(),       "ggplot")
      expect_s3_class(resid_gg(),     "ggplot")
    }
  )
})

test_that("anovaServer initializes without error on a fitted model", {
  m <- make_mocks()
  expect_no_error(
    shiny::testServer(
      anovaServer,
      args = list(model_module = m$model_module, data_module = m$data_module),
      { session$flushReact() }
    )
  )
})

test_that("correlationServer initializes without error", {
  m <- make_mocks()
  expect_no_error(
    shiny::testServer(
      correlationServer,
      args = list(data_module = m$data_module),
      { session$flushReact() }
    )
  )
})
