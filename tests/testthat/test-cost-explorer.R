test_that("ce_filter_hub() builds the right filter", {
  expect_null(ce_filter_hub("all"))
  expect_equal(ce_filter_hub("support")$Tags$MatchOptions, list("ABSENT"))
  expect_equal(ce_filter_hub("prod")$Tags$Values, list("prod"))
})

test_that("ce_filter_hub() errors for an invalid hub", {
  expect_snapshot(ce_filter_hub("invalid"), error = TRUE)
})
