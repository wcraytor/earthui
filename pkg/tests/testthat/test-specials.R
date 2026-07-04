# --- special_roles_ / special_default_for_ (Special column-role guesser) ----

test_that("special_roles_ returns the full set for appraiser modes only", {
  full <- special_roles_(TRUE)
  expect_true(all(c("living_area", "contract_date", "sale_type", "weight",
                    "no") %in% full))
  expect_identical(special_roles_(FALSE), c("no", "weight"))
})

test_that("special_default_for_ matches canonical names and synonyms", {
  tags <- special_roles_(TRUE)
  expect_equal(special_default_for_("living_area", tags), "living_area")
  expect_equal(special_default_for_("gla", tags), "living_area")
  expect_equal(special_default_for_("sqft", tags), "living_area")  # whole name
  expect_equal(special_default_for_("lot_sqft", tags), "lot_size")
  expect_equal(special_default_for_("lat", tags), "latitude")
  expect_equal(special_default_for_("saledate", tags), "contract_date")
})

test_that("sqft as a mere token does not claim living_area", {
  tags <- special_roles_(TRUE)
  # Remarks-parser feature columns (pr_* etc.) and auxiliary sqft columns
  # must not be auto-tagged as the living area
  expect_equal(special_default_for_("pr_sqft", tags), "no")
  expect_equal(special_default_for_("pr_outbuilding_sqft", tags), "no")
  expect_equal(special_default_for_("garage_sqft", tags), "no")
  expect_equal(special_default_for_("outbuilding_sqft", tags), "no")
})

test_that("generic tokens area/age only match whole names", {
  tags <- special_roles_(TRUE)
  expect_equal(special_default_for_("area", tags), "area")
  expect_equal(special_default_for_("area_id", tags), "no")
  expect_equal(special_default_for_("age", tags), "actual_age")
  expect_equal(special_default_for_("garage_spaces", tags), "no")
})
