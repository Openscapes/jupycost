test_that("get_default_prometheus_uid works", {
  mock_response <- list(
    list(name = "other", uid = "123"),
    list(name = "prometheus", uid = "abc", isDefault = FALSE)
  )

  local_mocked_bindings(
    req_perform = function(...) structure(list(), class = "httr2_response"),
    resp_body_json = function(...) mock_response,
    .package = "httr2"
  )

  expect_equal(get_default_prometheus_uid(), "abc")
})

test_that("get_prometheus_labels works", {
  mock_response <- c("label1", "label2")

  local_mocked_bindings(
    req_perform = function(...) structure(list(), class = "httr2_response"),
    resp_body_json = function(...) mock_response,
    resp_check_status = function(x) x,
    .package = "httr2"
  )

  local_mocked_bindings(
    get_default_prometheus_uid = function(...) "foo"
  )

  expect_equal(get_prometheus_labels(), mock_response)
})

test_that("get_prometheus_metrics works", {
  mock_response <- list(
    data = list(
      target = data.frame(col1 = 1),
      metric = "metric1",
      type = "gauge",
      help = "help text",
      unit = "bytes"
    )
  )

  local_mocked_bindings(
    req_perform = function(...) structure(list(), class = "httr2_response"),
    resp_body_json = function(...) mock_response,
    resp_check_status = function(x) x,
    .package = "httr2"
  )

  local_mocked_bindings(
    get_default_prometheus_uid = function(...) "foo"
  )

  result <- get_prometheus_metrics()
  expect_true(is.data.frame(result))
  expect_equal(names(result), c("col1", "metric", "type", "help", "unit"))
})

test_that("create_range_df works with provided data", {
  input <- list(
    data = list(
      result = list(
        metric = data.frame(job = "test_job"),
        values = list(
          data.frame(
            V1 = 1704067200,
            V2 = "123.45"
          )
        )
      )
    )
  )

  result <- create_range_df(input, "test_value")

  expect_equal(names(result), c("job", "date", "test_value"))
  expect_s3_class(result$date, "POSIXct")
  expect_type(result$test_value, "double")
  expect_equal(result$job, "test_job")
})

test_that("query_prometheus_range() works with nasa", {
  set_env_vars("nasa")
  skip_if_env_vars_not_set()

  ret <- query_prometheus_range(
    query = "max(dirsize_total_size_bytes)",
    step = "24h0m0s"
  )

  expect_type(ret, "list")
  expect_named(ret, c("status", "data"))
})

test_that("query_prometheus_range() works with nmfs", {
  set_env_vars("nmfs")
  skip_if_env_vars_not_set()

  ret <- query_prometheus_range(
    grafana_url = "https://grafana.nmfs-openscapes.2i2c.cloud",
    query = "max(dirsize_total_size_bytes)",
    step = "24h0m0s"
  )

  expect_type(ret, "list")
  expect_named(ret, c("status", "data"))
})
