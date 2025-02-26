test_that("get_daily_users() works with nasa", {
  set_env_vars("nasa")
  skip_if_offline()

  ret <- get_daily_users(start_time = "2025-01-01", end_time = "2025-01-10")

  expect_s3_class(ret, "data.frame")
  expect_named(ret, c("date", "namespace", "n_users"))
})

test_that("get_daily_users() works with nmfs", {
  set_env_vars("nmfs")
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
  skip_if_offline()

  ret <- get_hourly_users(
    grafana_url = "https://grafana.nmfs-openscapes.2i2c.cloud",
    start_time = "2025-01-01",
    end_time = "2025-01-10"
  )

  expect_s3_class(ret, "data.frame")
  expect_named(ret, c("date_time", "namespace", "n_users"))
})
