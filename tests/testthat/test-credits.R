raw_credits <- function() {
  month <- function(start, amount) {
    list(
      TimePeriod = list(Start = start),
      Total = list(UnblendedCost = list(Amount = amount))
    )
  }
  list(
    ResultsByTime = list(
      month("2026-08-01", "-1243.38"),
      month("2026-09-01", "-1243.22")
    )
  )
}

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
