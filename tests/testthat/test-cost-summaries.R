test_that("get_daily_usage_costs input validation works", {
  expect_snapshot(
    error = TRUE,
    get_daily_usage_costs(end_date = "not a date")
  )

  expect_snapshot(
    error = TRUE,
    get_daily_usage_costs(months_back = 13)
  )

  expect_snapshot(
    error = TRUE,
    get_daily_usage_costs(months_back = 1.5)
  )

  expect_snapshot(
    error = TRUE,
    get_daily_usage_costs(cost_type = "invalid")
  )
})

test_that("get_daily_usage_costs returns expected format", {
  mock_data <- data.frame(
    id = "unblended",
    date = as.Date("2023-01-01"),
    service = "foo",
    linked_account = "123",
    cost = 10
  )

  local_mocked_bindings(
    "aws_billing" = function(...) mock_data,
    .package = "sixtyfour"
  )

  result <- get_daily_usage_costs(
    end_date = as.Date("2024-06-01"),
    months_back = 1
  )

  expect_s3_class(result, "data.frame")
  expect_true(inherits(result$date, "Date"))
})

test_that("get_daily_usage_costs filters by cost_type correctly", {
  mock_data <- data.frame(
    id = c("unblended", "blended"),
    date = as.Date("2023-01-01"),
    service = "foo",
    linked_account = "123",
    cost = c(10, 20)
  )

  local_mocked_bindings(
    "aws_billing" = function(...) mock_data,
    .package = "sixtyfour"
  )

  unblended_result <- get_daily_usage_costs(cost_type = "unblended")
  expect_equal(nrow(unblended_result), 1)
  expect_equal(unblended_result$id, "unblended")

  all_result <- get_daily_usage_costs(cost_type = "all")
  expect_equal(nrow(all_result), 2)
  expect_equal(all_result$id, c("unblended", "blended"))
})

test_that("get_daily_usage_costs() works for real with nasa env vars", {
  set_env_vars("nasa")

  ret <- get_daily_usage_costs(months_back = 1)
  expect_s3_class(ret, "data.frame")
  expect_named(
    ret,
    c("id", "date", "service", "linked_account", "cost", "acronym")
  )
  expect_gt(nrow(ret), 0)
})

test_that("get_daily_usage_costs() works for real with nmfs env vars", {
  set_env_vars("nmfs")

  ret <- get_daily_usage_costs(months_back = 1)
  expect_s3_class(ret, "data.frame")
  expect_named(
    ret,
    c("id", "date", "service", "linked_account", "cost", "acronym")
  )
  expect_gt(nrow(ret), 0)
})
