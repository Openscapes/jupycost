test_that("get_daily_user_costs() returns one row per user unless by_usergroup = TRUE", {
  local_mock_cost_monitoring()

  expected <- dplyr::tibble(
    date = as.Date("2026-10-01"),
    hub = "prod",
    user = c("alice", "bob"),
    component = c("compute", "home storage"),
    cost = c(1.5, 0.25)
  )
  expect_equal(get_daily_user_costs("2026-10-01", "2026-10-02"), expected)

  by_group <- get_daily_user_costs(
    "2026-10-01",
    "2026-10-02",
    by_usergroup = TRUE
  )
  expect_equal(by_group$usergroup, c("group-a", "group-b", "multiple", "none"))
  expect_equal(dplyr::distinct(by_group[names(expected)]), expected)
})

test_that("get_daily_user_costs() queries each hub except support when hub = 'all'", {
  costs <- user_costs_fixture()
  costs$hub[4] <- "workshop"
  calls <- local_mock_cost_monitoring(
    costs,
    hubs = c("support", "prod", "workshop", "binder")
  )

  res <- get_daily_user_costs(
    start_date = "2026-10-01",
    end_date = "2026-10-02"
  )

  expect_equal(vapply(calls$queries, \(q) q$hub, ""), c("prod", "workshop"))
  expect_equal(res$hub, c("prod", "workshop"))
})

test_that("get_daily_user_costs() sends an inclusive end date and filters", {
  calls <- local_mock_cost_monitoring()

  get_daily_user_costs(
    start_date = "2026-09-01",
    end_date = "2026-10-01",
    hub = "workshop",
    component = "compute",
    user = "alice"
  )

  expect_equal(
    calls$queries,
    list(list(
      from = "2026-09-01",
      to = "2026-09-30",
      component = "compute",
      user = "alice",
      hub = "workshop"
    ))
  )
})

test_that("get_daily_user_costs() splits long date ranges into chunks", {
  calls <- local_mock_cost_monitoring()

  get_daily_user_costs(
    start_date = "2026-01-01",
    end_date = "2026-03-15",
    hub = "prod"
  )

  expect_equal(
    vapply(calls$queries, \(q) paste(q$from, q$to), ""),
    c(
      "2026-01-01 2026-01-31",
      "2026-02-01 2026-03-03",
      "2026-03-04 2026-03-14"
    )
  )
  expect_equal(calls$uid_lookups, 1)
})

test_that("get_daily_user_costs() handles an empty response", {
  local_mock_cost_monitoring(costs = user_costs_fixture()[0, ])

  res <- get_daily_user_costs(
    start_date = "2026-10-01",
    end_date = "2026-10-02"
  )

  expect_equal(nrow(res), 0)
  expect_named(res, c("date", "hub", "user", "component", "cost"))
  expect_s3_class(res$date, "Date")
})

test_that("get_daily_user_costs() requires start_date before end_date", {
  expect_snapshot(error = TRUE, {
    get_daily_user_costs(start_date = "2026-10-02", end_date = "2026-10-01")
  })
})

test_that("get_daily_user_costs() works against Grafana", {
  skip_on_cran()
  skip_if_env_vars_not_set()

  res <- get_daily_user_costs(
    start_date = Sys.Date() - 3,
    hub = "prod",
    component = "compute"
  )

  expect_named(res, c("date", "hub", "user", "component", "cost"))
  expect_all_true(res$hub == "prod")
  expect_all_true(res$component == "compute")
})
