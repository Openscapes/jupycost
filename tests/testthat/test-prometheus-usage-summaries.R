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

test_that("user_dir_info() works with nasa", {
  set_env_vars("nasa")
  skip_if_env_vars_not_set()
  skip_if_offline()

  ret <- user_dir_info()

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

test_that("user_dir_info() works with nmfs", {
  set_env_vars("nmfs")
  skip_if_env_vars_not_set()
  skip_if_offline()

  ret <- user_dir_info(
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
