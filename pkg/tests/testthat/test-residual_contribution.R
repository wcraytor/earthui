# Tests for estimate_residual_contribution() — the RCA residual-layer
# estimator for rare / excluded features.

make_planted_data <- function(n = 400, seed = 42) {
  set.seed(seed)
  df <- data.frame(
    gla  = round(runif(n, 1000, 3500)),
    lot  = round(runif(n, 3000, 20000)),
    beds = sample(2:5, n, replace = TRUE)
  )
  # rare flag: workshop on 6 sales, worth a planted $30,000
  df$workshop <- FALSE
  df$workshop[sample(n, 6)] <- TRUE
  # continuous rare-ish: outbuilding sqft on 25 sales at $40/sqft
  df$outbuilding_sqft <- 0
  idx <- sample(n, 25)
  df$outbuilding_sqft[idx] <- round(runif(25, 100, 900))
  df$price <- 200000 + 150 * df$gla + 4 * df$lot + 8000 * df$beds +
    30000 * df$workshop + 40 * df$outbuilding_sqft + rnorm(n, 0, 12000)
  df
}

test_that("joint residual regression recovers planted contributions", {
  df <- make_planted_data()
  r <- fit_earth(df, "price", c("gla", "lot", "beds"), nfold = 0)
  rc <- estimate_residual_contribution(r, df,
                                       c("workshop", "outbuilding_sqft"))
  expect_s3_class(rc, "earthUI_residual_contribution")
  tab <- rc$table
  w <- tab[tab$term == "workshop", ]
  o <- tab[tab$term == "outbuilding_sqft", ]
  # planted $30k lump and $40/sqft, wide tolerance (n=6 flag is noisy)
  expect_gt(w$estimate, 15000); expect_lt(w$estimate, 45000)
  expect_gt(o$estimate, 30);    expect_lt(o$estimate, 50)
  expect_equal(w$n_nonzero, 6)
  expect_equal(o$n_nonzero, 25)
})

test_that("flagged-sale residuals are reported for binary features", {
  df <- make_planted_data()
  r <- fit_earth(df, "price", c("gla", "lot", "beds"), nfold = 0)
  rc <- estimate_residual_contribution(r, df, "workshop")
  expect_named(rc$flagged, "workshop")
  expect_equal(nrow(rc$flagged$workshop), 6)
  # planted value should push flagged residuals systematically positive
  expect_gt(median(rc$flagged$workshop$residual), 10000)
})

test_that("low-n caution lands in the workfile note", {
  df <- make_planted_data()
  r <- fit_earth(df, "price", c("gla", "lot", "beds"), nfold = 0)
  rc <- estimate_residual_contribution(r, df, "workshop")
  expect_match(rc$note, "CAUTION")
  expect_match(rc$note, "workshop")
})

test_that("model predictors and unknown columns are rejected", {
  df <- make_planted_data()
  r <- fit_earth(df, "price", c("gla", "lot", "beds"), nfold = 0)
  expect_error(estimate_residual_contribution(r, df, "gla"),
               "Already model predictors")
  expect_error(estimate_residual_contribution(r, df, "no_such_col"),
               "Not in data")
})
