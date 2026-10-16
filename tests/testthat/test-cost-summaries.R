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
    get_daily_usage_costs(start_date = "2024-01-01", months_back = 2)
  )

  expect_snapshot(
    error = TRUE,
    get_daily_usage_costs(start_date = "2024-06-01", end_date = "2024-05-01")
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
    aws_billing_daily = function(...) mock_data
  )

  result <- get_daily_usage_costs(
    end_date = as.Date("2024-06-01"),
    months_back = 1
  )

  expect_s3_class(result, "data.frame")
  expect_true(inherits(result$date, "Date"))
})

test_that("get_daily_usage_costs passes cost_type to the billing query", {
  types <- character()
  local_mocked_bindings(
    aws_billing_daily = function(date_start, date_end, filter, cost_type) {
      types <<- c(types, cost_type)
      data.frame(
        id = "unblended",
        date = as.Date("2023-01-01"),
        service = "foo",
        linked_account = "123",
        cost = 10
      )
    }
  )

  get_daily_usage_costs(cost_type = "blended")
  get_daily_usage_costs(cost_type = "all")
  expect_equal(types, c("blended", "all"))
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
  mock_fn <- function(date_start, date_end, filter, cost_type) {
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
    aws_billing_daily = mock_fn
  )

  # Test that cluster filter and hub filter are both applied
  get_daily_usage_costs(cluster = "nmfs-openscapes", hub = "prod")

  filter_str <- deparse(filter_capture)

  # Should have both cluster name and hub name in filter
  expect_snapshot(filter_str)
})

test_that("get_daily_usage_costs filter structure is correct for support hub", {
  filter_capture <- NULL
  mock_fn <- function(date_start, date_end, filter, cost_type) {
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
    aws_billing_daily = mock_fn
  )

  # Test shared hub uses ABSENT match option
  get_daily_usage_costs(hub = "support")

  expect_snapshot(deparse(filter_capture))
})

test_that("get_daily_usage_costs cluster filter includes all required tag keys", {
  filter_capture <- NULL
  mock_fn <- function(date_start, date_end, filter, cost_type) {
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
    aws_billing_daily = mock_fn
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

  # Tagged sub-queries add filters beyond the two base ones; return nothing
  local_mocked_bindings(
    aws_billing_daily = function(date_start, date_end, filter, cost_type) {
      if (length(filter$And) > 2) mock_data[0, ] else mock_data
    }
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
    "core"
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
    aws_billing_daily = function(...) mock_data
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
    aws_billing_daily = function(...) mock_data
  )

  result <- get_daily_usage_costs()

  expect_true("service_component" %in% names(result))
  expect_type(result$service_component, "character")
})

test_that("get_daily_usage_costs passes start_date and end_date to the API", {
  dates <- NULL
  local_mocked_bindings(
    aws_billing_daily = function(date_start, date_end, filter, cost_type) {
      dates <<- c(date_start, date_end)
      data.frame(
        id = "unblended",
        date = "2024-01-01",
        service = "foo",
        linked_account = "123",
        cost = 10
      )
    }
  )

  get_daily_usage_costs(start_date = "2024-05-22", end_date = "2024-06-22")
  expect_equal(dates, c("2024-05-22", "2024-06-22"))
})

test_that("get_daily_usage_costs starts months_back months before end_date at the start of the month", {
  dates <- NULL
  local_mocked_bindings(
    aws_billing_daily = function(date_start, date_end, filter, cost_type) {
      dates <<- c(date_start, date_end)
      data.frame(
        id = "unblended",
        date = "2024-01-01",
        service = "foo",
        linked_account = "123",
        cost = 10
      )
    }
  )

  get_daily_usage_costs(end_date = "2024-06-15", months_back = 2)
  expect_equal(dates, c("2024-04-01", "2024-06-15"))
})

test_that("get_daily_usage_costs splits EC2 costs into components", {
  billing <- function(cost) {
    data.frame(
      id = "unblended",
      date = "2024-01-01",
      service = "EC2 - Other",
      linked_account = "123",
      cost = cost
    )
  }
  # Base query has 2 filters; home storage adds 2; core adds a Not clause
  local_mocked_bindings(
    aws_billing_daily = function(date_start, date_end, filter, cost_type) {
      has_not <- any(purrr::map_lgl(filter$And, \(x) "Not" %in% names(x)))
      if (has_not) {
        billing(3)
      } else if (length(filter$And) > 2) {
        billing(2)
      } else {
        billing(10)
      }
    }
  )

  result <- get_daily_usage_costs()

  expect_equal(
    result[c("service_component", "cost")],
    data.frame(
      service_component = c("compute", "home storage", "core"),
      cost = c(5, 2, 3)
    )
  )
})

test_that("split_service_components replaces split services and keeps the rest", {
  daily_ce <- data.frame(
    id = "unblended",
    date = as.Date("2024-01-01"),
    service = c("Amazon Backup", "EC2 - Other"),
    linked_account = "123",
    cost = c(1, 10),
    service_component = c("backup", "compute")
  )
  parts <- list(
    "home storage" = data.frame(
      id = "unblended",
      date = "2024-01-01",
      service = "EC2 - Other",
      linked_account = "123",
      cost = 4
    )
  )

  result <- split_service_components(daily_ce, parts)

  expect_named(result, names(daily_ce))
  expect_equal(result$service_component, c("backup", "compute", "home storage"))
  expect_equal(result$cost, c(1, 6, 4))
  expect_equal(sum(result$cost), sum(daily_ce$cost))
})

test_that("split_service_components returns input when there are no parts", {
  daily_ce <- data.frame(
    id = "unblended",
    date = as.Date("2024-01-01"),
    service = "EC2 - Other",
    linked_account = "123",
    cost = 10,
    service_component = "compute"
  )

  expect_equal(split_service_components(daily_ce, list()), daily_ce)
})

test_that("split_service_components warns when tagged costs exceed the total", {
  daily_ce <- data.frame(
    id = "unblended",
    date = as.Date("2024-01-01"),
    service = "EC2 - Other",
    linked_account = "123",
    cost = 10,
    service_component = "compute"
  )
  parts <- list(
    core = data.frame(
      id = "unblended",
      date = "2024-01-01",
      service = "EC2 - Other",
      linked_account = "123",
      cost = 12
    )
  )

  expect_snapshot(invisible(split_service_components(daily_ce, parts)))
})
