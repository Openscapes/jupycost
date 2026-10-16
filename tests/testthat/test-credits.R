test_that("get_credits_used returns positive credits per month", {
  local_mocked_bindings(ce_credits_raw = function(start, end) raw_credits())

  expect_equal(
    get_credits_used(end_date = "2026-10-01"),
    dplyr::tibble(
      month = as.Date(c("2026-08-01", "2026-09-01")),
      credits_used = c(1243.38, 1243.22)
    )
  )
})

test_that("get_credits_used queries from the start of the month months_back ago", {
  dates <- NULL
  local_mocked_bindings(
    ce_credits_raw = function(start, end) {
      dates <<- c(start, end)
      raw_credits()
    }
  )

  get_credits_used(end_date = "2026-10-16", months_back = 3)
  expect_equal(dates, c("2026-07-01", "2026-10-16"))
})

test_that("get_credits_used validates months_back", {
  expect_snapshot(get_credits_used(months_back = 14), error = TRUE)
})

test_that("get_credits_remaining returns one row per credit", {
  local_mocked_bindings(
    billing_credits_raw = function(start_date) raw_billing_credits()
  )

  result <- get_credits_remaining()

  expect_equal(result$credit_id, c("1", "2"))
  expect_equal(result$initial_amount, c(18000, 30000))
  expect_equal(result$remaining_amount, c(0, 18311.72))
  expect_equal(result$exhaust_date[[1]], as.POSIXct("2025-11-01", tz = "UTC"))
  expect_true(is.na(result$exhaust_date[[2]]))
})

test_that("get_credits_remaining validates months_back", {
  expect_snapshot(get_credits_remaining(months_back = 13), error = TRUE)
})
