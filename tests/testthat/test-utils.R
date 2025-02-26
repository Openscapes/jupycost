test_that("unsanitize_dir_names reverses sanitization", {
  expect_equal(unsanitize_dir_names("hello-2dworld"), "hello-world")
  expect_equal(unsanitize_dir_names("hello-2eworld"), "hello.world")
  expect_equal(unsanitize_dir_names("hello-40world"), "hello@world")
  expect_equal(unsanitize_dir_names("hello-5fworld"), "hello_world")
  expect_equal(
    unsanitize_dir_names("hello-2dworld-2etest-40example-5ffile"),
    "hello-world.test@example_file"
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
