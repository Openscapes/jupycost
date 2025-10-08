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

  expect_snapshot(
    error = TRUE,
    get_daily_usage_costs(hub = "invalid")
  )

  expect_snapshot(
    error = TRUE,
    get_daily_usage_costs(cluster = "invalid")
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
  skip_if_env_vars_not_set()

  ret <- get_daily_usage_costs(months_back = 1)
  expect_s3_class(ret, "data.frame")
  expect_named(
    ret,
    c(
      "id",
      "date",
      "service",
      "linked_account",
      "cost",
      "acronym",
      "service_component"
    )
  )
  expect_gt(nrow(ret), 0)
})

test_that("get_daily_usage_costs() works for real with nmfs env vars", {
  set_env_vars("nmfs")
  skip_if_env_vars_not_set()

  ret <- get_daily_usage_costs(months_back = 1)
  expect_s3_class(ret, "data.frame")
  expect_named(
    ret,
    c(
      "id",
      "date",
      "service",
      "linked_account",
      "cost",
      "acronym",
      "service_component"
    )
  )
  expect_gt(nrow(ret), 0)
})

test_that("get_daily_usage_costs correctly combines cluster and hub filters", {
  filter_capture <- NULL
  mock_fn <- function(start_date, end_date, filter) {
    filter_capture <<- filter
    data.frame(
      id = "unblended",
      date = as.Date("2024-01-01"),
      service = "Amazon EC2",
      linked_account = "123",
      cost = 10
    )
  }

  local_mocked_bindings(
    "aws_billing" = mock_fn,
    .package = "sixtyfour"
  )

  # Test that cluster filter and hub filter are both applied
  get_daily_usage_costs(cluster = "nmfs-openscapes", hub = "prod")

  filter_str <- deparse(filter_capture)

  # Should have both cluster name and hub name in filter
  expect_snapshot(filter_str)
})

test_that("get_daily_usage_costs filter structure is correct for support hub", {
  filter_capture <- NULL
  mock_fn <- function(start_date, end_date, filter) {
    filter_capture <<- filter
    data.frame(
      id = "unblended",
      date = as.Date("2024-01-01"),
      service = "Amazon EC2",
      linked_account = "123",
      cost = 10
    )
  }

  local_mocked_bindings(
    "aws_billing" = mock_fn,
    .package = "sixtyfour"
  )

  # Test shared hub uses ABSENT match option
  get_daily_usage_costs(hub = "support")

  expect_snapshot(deparse(filter_capture))
})

test_that("get_daily_usage_costs cluster filter includes all required tag keys", {
  filter_capture <- NULL
  mock_fn <- function(start_date, end_date, filter) {
    filter_capture <<- filter
    data.frame(
      id = "unblended",
      date = as.Date("2024-01-01"),
      service = "Amazon EC2",
      linked_account = "123",
      cost = 10
    )
  }

  local_mocked_bindings(
    "aws_billing" = mock_fn,
    .package = "sixtyfour"
  )

  # Test openscapes cluster
  get_daily_usage_costs(cluster = "openscapeshub")

  expect_snapshot(deparse(filter_capture))

  # Test nmfs-openscapes cluster
  get_daily_usage_costs(cluster = "nmfs-openscapes")

  expect_snapshot(deparse(filter_capture))
})

test_that("get_daily_usage_costs service_component mapping handles all known services", {
  mock_data <- data.frame(
    id = "unblended",
    date = as.Date("2024-01-01"),
    service = c(
      "AWS Backup",
      "EC2 - Other",
      "Amazon Elastic Compute Cloud - Compute",
      "Amazon Elastic Container Service for Kubernetes",
      "Amazon Elastic File System",
      "Amazon Elastic Load Balancing",
      "Amazon Simple Storage Service",
      "Amazon Virtual Private Cloud"
    ),
    linked_account = "123",
    cost = seq(10, 80, 10)
  )

  local_mocked_bindings(
    "aws_billing" = function(...) mock_data,
    .package = "sixtyfour"
  )

  result <- get_daily_usage_costs()

  expect_equal(
    result$service_component[result$service == "AWS Backup"],
    "backup"
  )
  expect_equal(
    result$service_component[result$service == "EC2 - Other"],
    "compute"
  )
  expect_equal(
    result$service_component[
      result$service == "Amazon Elastic Compute Cloud - Compute"
    ],
    "compute"
  )
  expect_equal(
    result$service_component[
      result$service == "Amazon Elastic Container Service for Kubernetes"
    ],
    "fixed"
  )
  expect_equal(
    result$service_component[result$service == "Amazon Elastic File System"],
    "home storage"
  )
  expect_equal(
    result$service_component[result$service == "Amazon Elastic Load Balancing"],
    "networking"
  )
  expect_equal(
    result$service_component[result$service == "Amazon Simple Storage Service"],
    "object storage"
  )
  expect_equal(
    result$service_component[result$service == "Amazon Virtual Private Cloud"],
    "networking"
  )
})

test_that("get_daily_usage_costs service_component defaults to 'other' for unmapped services", {
  mock_data <- data.frame(
    id = "unblended",
    date = as.Date("2024-01-01"),
    service = c("Unknown Service 1", "Random Service", "Mystery Service"),
    linked_account = "123",
    cost = c(5, 10, 15)
  )

  local_mocked_bindings(
    "aws_billing" = function(...) mock_data,
    .package = "sixtyfour"
  )

  result <- get_daily_usage_costs()

  expect_true(all(result$service_component == "other"))
  expect_equal(nrow(result), 3)
})

test_that("get_daily_usage_costs includes service_component column in output", {
  mock_data <- data.frame(
    id = "unblended",
    date = as.Date("2024-01-01"),
    service = "Amazon EC2",
    linked_account = "123",
    cost = 10
  )

  local_mocked_bindings(
    "aws_billing" = function(...) mock_data,
    .package = "sixtyfour"
  )

  result <- get_daily_usage_costs()

  expect_true("service_component" %in% names(result))
  expect_type(result$service_component, "character")
})
