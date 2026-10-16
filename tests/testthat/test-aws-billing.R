raw_ce <- function(metric, amount = "2.5", token = NULL) {
  group <- function(service, amount) {
    list(
      Keys = list(service, "123"),
      Metrics = rlang::set_names(list(list(Amount = amount)), metric)
    )
  }
  list(
    ResultsByTime = list(
      list(
        TimePeriod = list(Start = "2024-01-01"),
        Groups = list(group("EC2 - Other", amount), group("New Service", "1"))
      )
    ),
    NextPageToken = token
  )
}

test_that("aws_billing_daily sends one request per metric needed", {
  requested <- character()
  local_mocked_bindings(
    aws_billing_raw = function(date_start, metrics, ...) {
      requested <<- c(requested, metrics)
      raw_ce(metrics)
    }
  )

  aws_billing_daily("2024-01-01", "2024-01-02", cost_type = "blended")
  expect_equal(requested, "BlendedCost")

  requested <- character()
  aws_billing_daily("2024-01-01", "2024-01-02", cost_type = "unblended")
  expect_equal(requested, "UnblendedCost")

  requested <- character()
  aws_billing_daily("2024-01-01", "2024-01-02", cost_type = "all")
  expect_equal(requested, c("UnblendedCost", "BlendedCost"))
})

test_that("aws_billing_daily returns the columns of sixtyfour::aws_billing()", {
  local_mocked_bindings(
    aws_billing_raw = function(date_start, metrics, ...) raw_ce(metrics)
  )

  result <- aws_billing_daily("2024-01-01", "2024-01-02", cost_type = "all")

  expect_named(
    result,
    c("id", "date", "service", "linked_account", "cost", "acronym")
  )
  expect_equal(result$id, rep(c("unblended", "blended"), each = 2))
  expect_equal(result$cost, c(2.5, 1, 2.5, 1))
  expect_equal(result$acronym, rep(c("EC2 - Other", "New Service"), 2))
})

test_that("aws_billing_daily returns an empty data frame for no results", {
  local_mocked_bindings(
    aws_billing_raw = function(...) list(ResultsByTime = list())
  )

  result <- aws_billing_daily("2024-01-01", "2024-01-02")

  expect_equal(nrow(result), 0)
  expect_named(
    result,
    c("id", "date", "service", "linked_account", "cost", "acronym")
  )
})

test_that("aws_billing_daily errors when the response is paginated", {
  local_mocked_bindings(
    aws_billing_raw = function(date_start, metrics, ...) {
      raw_ce(metrics, token = "abc")
    }
  )

  expect_snapshot(
    aws_billing_daily("2024-01-01", "2024-01-02"),
    error = TRUE
  )
})
