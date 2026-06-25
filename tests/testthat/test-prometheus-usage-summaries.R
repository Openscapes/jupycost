test_that("get_daily_users() works with nasa", {
  set_env_vars("nasa")
  skip_if_env_vars_not_set()
  skip_if_offline()

  ret <- get_daily_users(start_time = "2025-01-01", end_time = "2025-01-10")

  expect_s3_class(ret, "data.frame")
  expect_named(ret, c("date", "namespace", "n_users"))
})

test_that("get_daily_users() works with nmfs", {
  set_env_vars("nmfs")
  skip_if_env_vars_not_set()
  skip_if_offline()

  ret <- get_daily_users(
    grafana_url = "https://grafana.nmfs-openscapes.2i2c.cloud",
    start_time = "2025-01-01",
    end_time = "2025-01-10"
  )

  expect_s3_class(ret, "data.frame")
  expect_named(ret, c("date", "namespace", "n_users"))
})

test_that("get_hourly_users() works with nasa", {
  set_env_vars("nasa")
  skip_if_env_vars_not_set()
  skip_if_offline()

  ret <- get_hourly_users(
    grafana_url = "https://grafana.openscapes.2i2c.cloud",
    start_time = "2025-01-01",
    end_time = "2025-01-10"
  )

  expect_s3_class(ret, "data.frame")
  expect_named(ret, c("date_time", "namespace", "n_users"))
})

test_that("get_hourly_users() works with nmfs", {
  set_env_vars("nmfs")
  skip_if_env_vars_not_set()
  skip_if_offline()

  ret <- get_hourly_users(
    grafana_url = "https://grafana.nmfs-openscapes.2i2c.cloud",
    start_time = "2025-01-01",
    end_time = "2025-01-10"
  )

  expect_s3_class(ret, "data.frame")
  expect_named(ret, c("date_time", "namespace", "n_users"))
})

test_that("user_dir_snapshot() works with nasa", {
  set_env_vars("nasa")
  skip_if_env_vars_not_set()
  skip_if_offline()

  ret <- user_dir_snapshot()

  expect_s3_class(ret, "data.frame")
  expect_named(
    ret,
    c(
      "date",
      "namespace",
      "directory",
      "last_accessed",
      "n_files",
      "dirsize_mb",
      "percent_total_size"
    )
  )
})

test_that("user_dir_snapshot() works with nmfs", {
  set_env_vars("nmfs")
  skip_if_env_vars_not_set()
  skip_if_offline()

  ret <- user_dir_snapshot(
    grafana_url = "https://grafana.nmfs-openscapes.2i2c.cloud"
  )

  expect_s3_class(ret, "data.frame")
  expect_named(
    ret,
    c(
      "date",
      "namespace",
      "directory",
      "last_accessed",
      "n_files",
      "dirsize_mb",
      "percent_total_size"
    )
  )
})

test_that("user_dir_snapshot works with mocked responses", {
  mock_date_response <- list(
    data = list(
      result = list(
        metric = data.frame(namespace = "test", directory = "user-2ddir"),
        value = list(c("1704067200", "123.45"))
      )
    )
  )

  mock_size_response <- list(
    data = list(
      result = list(
        metric = data.frame(namespace = "test", directory = "user-2ddir"),
        value = list(c("1704067200", "1000000"))
      )
    )
  )

  mock_files_response <- list(
    data = list(
      result = list(
        metric = data.frame(namespace = "test", directory = "user-2ddir"),
        value = list(c("1704067200", "100"))
      )
    )
  )

  local_mocked_bindings(
    req_perform = function(...) structure(list(), class = "httr2_response"),
    resp_body_json = function(...) {
      parent <- parent.frame()
      if (grepl("mtime", parent$query)) {
        return(mock_date_response)
      }
      if (grepl("size_bytes", parent$query)) {
        return(mock_size_response)
      }
      if (grepl("entries_count", parent$query)) return(mock_files_response)
    },
    resp_check_status = function(x) x,
    .package = "httr2"
  )

  local_mocked_bindings(
    get_default_prometheus_uid = function(...) "foo"
  )

  result <- user_dir_snapshot(time = as.POSIXct("2024-01-01"))

  expect_s3_class(result, "data.frame")
  expect_named(
    result,
    c(
      "date",
      "namespace",
      "directory",
      "last_accessed",
      "n_files",
      "dirsize_mb",
      "percent_total_size"
    )
  )
  expect_equal(result$namespace, "test")
  expect_equal(result$directory, "user-dir")
  expect_equal(result$n_files, 100)
  expect_equal(result$dirsize_mb, 1)
  expect_equal(result$percent_total_size, 100)
})

test_that("dir_sizes() works with nasa", {
  set_env_vars("nasa")
  skip_if_env_vars_not_set()
  skip_if_offline()

  ret <- dir_sizes(start_time = "2025-01-01", end_time = "2025-01-10")

  expect_s3_class(ret, "data.frame")
  expect_named(ret, c("namespace", "date", "dirsize_mb"))
})

test_that("dir_sizes() works with nmfs", {
  set_env_vars("nmfs")
  skip_if_env_vars_not_set()
  skip_if_offline()

  ret <- dir_sizes(
    grafana_url = "https://grafana.nmfs-openscapes.2i2c.cloud",
    start_time = "2025-01-01",
    end_time = "2025-01-10"
  )

  expect_s3_class(ret, "data.frame")
  expect_named(ret, c("namespace", "date", "dirsize_mb"))
})

test_that("dir_sizes() works with by_user = TRUE", {
  mock_response <- list(
    data = list(
      result = list(
        metric = data.frame(namespace = "test", directory = "user-2ddir"),
        values = list(
          data.frame(
            V1 = 1704067200,
            V2 = "1000000"
          )
        )
      )
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

  result <- dir_sizes(
    by_user = TRUE,
    start_time = as.POSIXct("2024-01-01"),
    end_time = as.POSIXct("2024-01-01")
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("namespace", "directory", "date", "dirsize_mb"))
  expect_equal(result$namespace, "test")
  expect_equal(result$directory, "user-dir")
  expect_equal(result$dirsize_mb, 1)
})

test_that("get_workshop_users() works with nasa (by_user = FALSE)", {
  set_env_vars("nasa")
  skip_if_env_vars_not_set()
  skip_if_offline()

  ret <- get_workshop_users(start_time = "2025-01-01", end_time = "2025-01-10")

  expect_s3_class(ret, "data.frame")
  expect_named(ret, c("namespace", "n_users", "start_time", "end_time"))
  expect_type(ret$n_users, "integer")
  expect_s3_class(ret$start_time, "Date")
  expect_s3_class(ret$end_time, "Date")
})

test_that("get_workshop_users() works with nasa (by_user = TRUE)", {
  set_env_vars("nasa")
  skip_if_env_vars_not_set()
  skip_if_offline()

  ret <- get_workshop_users(
    start_time = "2025-01-01",
    end_time = "2025-01-10",
    by_user = TRUE
  )

  expect_s3_class(ret, "data.frame")
  expect_named(
    ret,
    c(
      "namespace",
      "directory",
      "first_seen",
      "last_seen",
      "start_time",
      "end_time"
    )
  )
  expect_type(ret$directory, "character")
  expect_s3_class(ret$first_seen, "Date")
  expect_s3_class(ret$last_seen, "Date")
  expect_true(all(ret$first_seen <= ret$last_seen, na.rm = TRUE))
})

test_that("get_workshop_users() works with nmfs", {
  set_env_vars("nmfs")
  skip_if_env_vars_not_set()
  skip_if_offline()

  ret <- get_workshop_users(
    grafana_url = "https://grafana.nmfs-openscapes.2i2c.cloud",
    start_time = "2025-01-01",
    end_time = "2025-01-10",
    namespace = NULL
  )

  expect_s3_class(ret, "data.frame")
  expect_named(ret, c("namespace", "n_users", "start_time", "end_time"))
  expect_type(ret$n_users, "integer")
})

test_that("get_workshop_users() works with mocked response (by_user = FALSE)", {
  mock_response <- list(
    data = list(
      result = list(
        metric = data.frame(namespace = "workshop"),
        value = list(c("1704067200", "5"))
      )
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

  result <- get_workshop_users(
    start_time = as.Date("2024-01-01"),
    end_time = as.Date("2024-01-31")
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("namespace", "n_users", "start_time", "end_time"))
  expect_equal(result$namespace, "workshop")
  expect_equal(result$n_users, 5L)
  expect_equal(result$start_time, as.Date("2024-01-01"))
  expect_equal(result$end_time, as.Date("2024-01-31"))
})

test_that("get_workshop_users() works with mocked response (by_user = TRUE)", {
  mock_dirs <- list(
    data = list(
      result = list(
        metric = data.frame(
          namespace = c("workshop", "workshop"),
          directory = c("user-2done", "user-2dtwo")
        ),
        value = list(
          c("1704067200", "1000000"),
          c("1704067200", "2000000")
        )
      )
    )
  )

  # first_seen: user-one was first seen 2024-01-01, user-two on 2024-01-06
  mock_first <- list(
    data = list(
      result = list(
        metric = data.frame(
          namespace = c("workshop", "workshop"),
          directory = c("user-2done", "user-2dtwo")
        ),
        value = list(
          c("1704067200", "1704067200"),
          c("1704067200", "1704499200")
        )
      )
    )
  )

  # last_seen: user-one last seen 2024-01-20, user-two on 2024-01-31
  mock_last <- list(
    data = list(
      result = list(
        metric = data.frame(
          namespace = c("workshop", "workshop"),
          directory = c("user-2done", "user-2dtwo")
        ),
        value = list(
          c("1704067200", "1705708800"),
          c("1704067200", "1706659200")
        )
      )
    )
  )

  local_mocked_bindings(
    req_perform = function(...) structure(list(), class = "httr2_response"),
    resp_body_json = function(...) {
      q <- parent.frame()$query
      if (!grepl("timestamp", q)) {
        return(mock_dirs)
      }
      if (grepl("min_over_time", q)) {
        return(mock_first)
      }
      mock_last
    },
    resp_check_status = function(x) x,
    .package = "httr2"
  )

  local_mocked_bindings(
    get_default_prometheus_uid = function(...) "foo"
  )

  result <- get_workshop_users(
    by_user = TRUE,
    start_time = as.Date("2024-01-01"),
    end_time = as.Date("2024-01-31")
  )

  expect_s3_class(result, "data.frame")
  expect_named(
    result,
    c(
      "namespace",
      "directory",
      "first_seen",
      "last_seen",
      "start_time",
      "end_time"
    )
  )
  expect_equal(result$namespace, c("workshop", "workshop"))
  # Sanitized directory names ("-2d" -> "-") should be reversed
  expect_equal(result$directory, c("user-one", "user-two"))
  expect_s3_class(result$first_seen, "Date")
  expect_s3_class(result$last_seen, "Date")
  expect_equal(result$first_seen, as.Date(c("2024-01-01", "2024-01-06")))
  expect_equal(result$last_seen, as.Date(c("2024-01-20", "2024-01-31")))
  expect_true(all(result$first_seen <= result$last_seen))
  expect_equal(result$start_time, rep(as.Date("2024-01-01"), 2))
  expect_equal(result$end_time, rep(as.Date("2024-01-31"), 2))
})

test_that("dir_sizes() works with by_user = FALSE", {
  mock_response <- list(
    data = list(
      result = list(
        metric = data.frame(namespace = "test"),
        values = list(
          data.frame(
            V1 = 1704067200,
            V2 = "1000000"
          )
        )
      )
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

  result <- dir_sizes(
    by_user = FALSE,
    start_time = as.POSIXct("2024-01-01"),
    end_time = as.POSIXct("2024-01-01")
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("namespace", "date", "dirsize_mb"))
  expect_equal(result$namespace, "test")
  expect_equal(result$dirsize_mb, 1)
})
