test_that("unsanitize_dir_names reverses sanitization", {
  expect_equal(unsanitize_dir_names("hello-2dworld"), "hello-world")
  expect_equal(unsanitize_dir_names("hello-2eworld"), "hello.world")
  expect_equal(unsanitize_dir_names("hello-40world"), "hello@world")
  expect_equal(unsanitize_dir_names("hello-5fworld"), "hello_world")
  expect_equal(
    unsanitize_dir_names(c(
      "hello-2dworld-2etest-40example-5ffile",
      "hello-world-2etest-40example_file"
    )),
    c("hello-world.test@example_file", "hello-world.test@example_file")
  )
})

test_that("unsanitize_dir_names decodes all patterns atomically", {
  # A username containing a literal "-2e" sanitizes to "-2d2e" ("-" -> "-2d",
  # "2e" is kept as-is). Sequential gsubs would decode "-2d" to "-", then
  # incorrectly decode the resulting "-2e" to ".", giving "-." instead of "-2e".
  expect_equal(unsanitize_dir_names("user-2d2etest"), "user-2etest")

  # Vectorised: should handle multiple strings correctly
  expect_equal(
    unsanitize_dir_names(c("user-2d2etest", "hello-2dworld")),
    c("user-2etest", "hello-world")
  )
})

test_that("check_valid_date accepts valid date inputs", {
  expect_equal(check_valid_date(Sys.Date()), Sys.Date())
  expect_equal(check_valid_date("2024-01-01"), as.Date("2024-01-01"))
  expect_equal(
    check_valid_date(as.POSIXct("2024-01-01")),
    as.Date("2024-01-01")
  )
})

test_that("check_valid_date errors informatively for invalid inputs", {
  expect_snapshot(check_valid_date("not a date"), error = TRUE)
  expect_snapshot(check_valid_date("2024-35-19"), error = TRUE)
  expect_snapshot(check_valid_date(c("2024-01-01", "2024-01-02")), error = TRUE)
  expect_snapshot(check_valid_date(NA_character_), error = TRUE)
  expect_snapshot(check_valid_date(NULL), error = TRUE)
})

test_that("check_valid_date includes argument name in error", {
  expect_snapshot(check_valid_date("not a date", arg = "my_date"), error = TRUE)
})

test_that("time_string formats dates correctly for prometheus", {
  test_datetime <- as.POSIXct("2024-01-01 12:34:56", tz = "UTC")
  test_date <- as.Date("2024-01-01")
  test_posixlt <- as.POSIXlt("2024-01-01 12:34:56", tz = "UTC")

  expect_equal(time_string(test_datetime), "2024-01-01T12:34:56Z")
  expect_equal(time_string(test_date), "2024-01-01T00:00:00Z")
  expect_equal(time_string(test_posixlt), "2024-01-01T12:34:56Z")
  expect_equal(time_string("2024-01-01 12:34:56"), "2024-01-01T12:34:56Z")
})

test_that("time_string handles timezone conversion correctly", {
  est_time <- as.POSIXct("2024-01-01 07:00:00", tz = "America/New_York")
  expect_equal(time_string(est_time), "2024-01-01T12:00:00Z")
})

test_that("time_string errors on invalid inputs", {
  expect_snapshot(error = TRUE, time_string("not a date"))
  expect_snapshot(error = TRUE, time_string(NULL))
  expect_snapshot(error = TRUE, time_string(NA))
  expect_snapshot(error = TRUE, time_string(42))
})
