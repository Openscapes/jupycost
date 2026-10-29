#' Get daily costs per user
#'
#' Get the daily cost of each user on a hub, from 2i2c's
#' [jupyterhub-cost-monitoring](https://github.com/2i2c-org/jupyterhub-cost-monitoring)
#' service. This is the data behind the "User cloud costs" Grafana dashboard.
#'
#' 2i2c estimates per-user costs by splitting each day's AWS cost of a
#' component among users, in proportion to their share of that component's
#' usage, as measured by Prometheus. Only the `"compute"` and
#' `"home storage"` components are split among users. Shared costs, such as
#' `"core"`, `"networking"` and `"object storage"`, are not included, so
#' per-user costs add up to less than the totals from
#' [get_daily_usage_costs()].
#'
#' @param start_date A Date object (or string in a standard unambiguous date
#'   format) representing the first day of the desired date range.
#'   **Inclusive**. Default is 30 days before `end_date`.
#' @param end_date A Date object (or string in a standard unambiguous date
#'   format) representing the end of the desired date range. **Exclusive**,
#'   as in [get_daily_usage_costs()]. The default, `Sys.Date()`, therefore
#'   excludes today's (incomplete) costs.
#' @param hub Name of the hub (for example `"prod"`, `"staging"` or
#'   `"workshop"`) to get costs for, or `"all"` (default) for all user-facing
#'   hubs. With `"all"`, each hub is queried separately (see Details).
#' @param component Which component to get costs for: `"all"` (default),
#'   `"compute"` or `"home storage"`.
#' @param user Optional. Username of a single user to get costs for.
#' @param by_usergroup If `FALSE` (default), return one row per user,
#'   hub, component and day. If `TRUE`, return one row per user group the user
#'   belongs to, with a `usergroup` column. See Details.
#' @inheritParams query_prometheus_range
#'
#' @details
#' Without a hub filter, the cost monitoring service splits each component's
#' cost for the whole cluster among all users, ignoring that hubs have
#' different costs. For example, users on a lightly used hub are assigned
#' part of the compute costs of a busy workshop hub. So with `hub = "all"`,
#' each hub is queried separately, which makes the costs per user add up to each
#' hub's costs.
#'
#' With `by_usergroup = TRUE`, the cost of a user who belongs to more than one
#' group is repeated for each of their groups, as well as in a group called
#' `"multiple"`. Users who belong to no group are in a group called `"none"`.
#' Summing costs across groups therefore overcounts the costs of users in
#' more than one group.
#'
#' @returns
#' A data frame with columns `date`, `hub`, `user`, `component`, `cost`
#' (in USD), and `usergroup` if `by_usergroup = TRUE`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Per-user costs on the prod hub for September 2026
#' get_daily_user_costs(
#'   start_date = "2026-09-01",
#'   end_date = "2026-10-01",
#'   hub = "prod"
#' )
#'
#' # Total cost per user and component over the last 30 days
#' get_daily_user_costs() |>
#'   dplyr::summarise(cost = sum(cost), .by = c(user, component))
#'
#' # Costs on the NMFS hub
#' set_env_vars("nmfs")
#' get_daily_user_costs(
#'   grafana_url = "https://grafana.nmfs-openscapes.2i2c.cloud"
#' )
#' }
get_daily_user_costs <- function(
  start_date = end_date - 30,
  end_date = Sys.Date(),
  hub = "all",
  component = c("all", "compute", "home storage"),
  user = NULL,
  by_usergroup = FALSE,
  grafana_url = "https://grafana.openscapes.2i2c.cloud",
  grafana_token = Sys.getenv("GRAFANA_TOKEN")
) {
  end_date <- check_valid_date(end_date)
  start_date <- check_valid_date(start_date)
  component <- rlang::arg_match(component)

  if (!rlang::is_string(hub)) {
    cli::cli_abort("{.arg hub} must be a single string.")
  }
  if (!is.null(user) && !rlang::is_string(user)) {
    cli::cli_abort("{.arg user} must be a single string or {.code NULL}.")
  }
  if (!rlang::is_bool(by_usergroup)) {
    cli::cli_abort("{.arg by_usergroup} must be {.code TRUE} or {.code FALSE}.")
  }

  if (start_date >= end_date) {
    cli::cli_abort(
      "{.arg start_date} ({start_date}) must be before {.arg end_date} ({end_date})."
    )
  }

  query <- list(
    from = format(start_date, "%Y-%m-%d"),
    # The cost monitoring service treats `to` as inclusive
    to = format(end_date - 1, "%Y-%m-%d"),
    component = component,
    user = user
  )

  # Without a hub filter, the service splits each component's cluster-wide
  # cost by users' cluster-wide usage share, ignoring that hubs have
  # different costs. Querying each hub separately attributes costs correctly.
  if (hub == "all") {
    hubs <- cost_monitoring_get(
      grafana_url,
      grafana_token,
      "hub-names",
      query[c("from", "to")]
    )
    hubs <- setdiff(unlist(hubs), c("support", "binder"))
  } else {
    hubs <- hub
  }

  # The service queries compute usage from Prometheus at a 5 minute step, so
  # ranges over about 38 days exceed Prometheus' 11,000 point limit and fail
  chunks <- date_chunks(start_date, end_date, max_days = 31)

  res <- list()
  for (h in hubs) {
    for (i in seq_len(nrow(chunks))) {
      chunk_query <- query
      chunk_query$from <- format(chunks$start[i], "%Y-%m-%d")
      chunk_query$to <- format(chunks$end[i] - 1, "%Y-%m-%d")
      res <- c(
        res,
        list(cost_monitoring_get(
          grafana_url,
          grafana_token,
          "costs-per-user",
          c(chunk_query, hub = h)
        ))
      )
    }
  }
  res <- purrr::list_rbind(Filter(\(x) NROW(x) > 0, res))

  if (NROW(res) == 0) {
    res <- data.frame(
      date = character(),
      hub = character(),
      user = character(),
      component = character(),
      value = numeric(),
      usergroup = character()
    )
  }

  out <- res |>
    dplyr::as_tibble() |>
    dplyr::transmute(
      date = as.Date(.data$date),
      .data$hub,
      .data$user,
      .data$component,
      cost = as.numeric(.data$value),
      .data$usergroup
    )

  if (by_usergroup) {
    return(out)
  }

  out |>
    dplyr::select(-"usergroup") |>
    dplyr::distinct()
}

date_chunks <- function(start_date, end_date, max_days) {
  starts <- seq(start_date, end_date - 1, by = max_days)
  data.frame(start = starts, end = c(starts[-1], end_date))
}

cost_monitoring_get <- function(grafana_url, grafana_token, path, query) {
  uid <- get_grafana_datasource_uid(
    grafana_url,
    grafana_token,
    type = "yesoreyeram-infinity-datasource"
  )

  httr2::request(grafana_url) |>
    httr2::req_options(http_version = 2) |>
    httr2::req_url_path("/api/datasources/proxy/uid", uid, path) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_auth_bearer_token(grafana_token) |>
    httr2::req_perform() |>
    httr2::resp_check_status() |>
    httr2::resp_body_json(simplifyVector = TRUE)
}

get_grafana_datasource_uid <- function(grafana_url, grafana_token, type) {
  body <- httr2::request(grafana_url) |>
    httr2::req_url_path("/api/datasources/") |>
    httr2::req_auth_bearer_token(grafana_token) |>
    httr2::req_perform() |>
    httr2::resp_check_status() |>
    httr2::resp_body_json()

  ds <- Filter(\(x) identical(x$type, type), body)

  if (length(ds) == 0) {
    cli::cli_abort(
      "No Grafana datasource of type {.val {type}} found at {.url {grafana_url}}."
    )
  }

  ds[[1]][["uid"]]
}
