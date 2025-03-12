#' Get the default Prometheus UID from Grafana
#'
#' @param grafana_url url to a Grafana instance. Default
#'        `"https://grafana.openscapes.2i2c.cloud"`
#' @param grafana_token Your Grafana API token.
#'
#' @returns A string containing the UID of the Prometheus datasource.
#'
#' @noRd
get_default_prometheus_uid <- function(
  grafana_url = "https://grafana.openscapes.2i2c.cloud",
  grafana_token = Sys.getenv("GRAFANA_TOKEN")
) {
  api_url <- glue::glue("{grafana_url}/")

  res <- httr2::request(grafana_url) |>
    httr2::req_url_path("/api/datasources/") |>
    httr2::req_auth_bearer_token(grafana_token) |>
    httr2::req_perform()

  body <- httr2::resp_body_json(res)

  # Could also check if isDefault, but there is only one and it is isDefault = FALSE
  Filter(\(x) x$name == "prometheus", body)[[1]][["uid"]]
}

#  get_homedir_dashboard <- function(
#     grafana_url = "https://grafana.openscapes.2i2c.cloud",
#     grafana_token = Sys.getenv("GRAFANA_TOKEN")
#   ) {
#     ret <- httr2::request(grafana_url) |>
#       httr2::req_url_path("/api/dashboards/uid", "bd232539-52d0-4435-8a62-fe637dc822be") |>
#       httr2::req_auth_bearer_token(grafana_token) |>
#       httr2::req_perform() |>
#       httr2::resp_check_status() |>
#       httr2::resp_body_json()

#     ret
#   }

#' Get a vector of labels available from Prometheus
#'
#' @inheritParams query_prometheus_range
#'
#' @return vector of labels
#' @export
get_prometheus_labels <- function(
  grafana_url = "https://grafana.openscapes.2i2c.cloud",
  grafana_token = Sys.getenv("GRAFANA_TOKEN")
) {
  prometheus_uid <- get_default_prometheus_uid(grafana_url, grafana_token)
  resp <- httr2::request(grafana_url) |>
    httr2::req_url_path(
      "/api/datasources/proxy/uid",
      prometheus_uid,
      "api/v1/labels"
    ) |>
    httr2::req_auth_bearer_token(grafana_token) |>
    httr2::req_perform() |>
    httr2::resp_check_status() |>
    httr2::resp_body_json(simplifyVector = TRUE, simplifyDataFrame = TRUE)

  resp$data
}

#' Get a data.frame of metrics available from Prometheus
#'
#' @inheritParams query_prometheus_range
#'
#' @return data.frame of metrics
#' @export
get_prometheus_metrics <- function(
  grafana_url = "https://grafana.openscapes.2i2c.cloud",
  grafana_token = Sys.getenv("GRAFANA_TOKEN")
) {
  prometheus_uid <- get_default_prometheus_uid(grafana_url, grafana_token)
  resp <- httr2::request(grafana_url) |>
    httr2::req_url_path(
      "/api/datasources/proxy/uid",
      prometheus_uid,
      "api/v1/targets/metadata"
    ) |>
    httr2::req_auth_bearer_token(grafana_token) |>
    httr2::req_perform() |>
    httr2::resp_check_status()

  ret <- resp |>
    httr2::resp_body_json(simplifyVector = TRUE, simplifyDataFrame = TRUE)

  cbind(
    ret$data$target,
    metric = ret$data$metric,
    type = ret$data$type,
    help = ret$data$help,
    unit = ret$data$unit
  )
}

#' Query Prometheus for an instant in time
#'
#' @inheritParams query_prometheus_range
#' @param time Date or date-time object, or character of the form
#'    "YYYY-MM-DD HH:MM:SS". Time components are optional. Default is current
#'    time (`Sys.time()`). If a `POSIXt` object, it will be converted
#'    to UTC, if a `Date` or `character` object, it will be assumed to be `UTC`
#'
#' @return List containing the response from Prometheus, in the
#'    [instant vector format](https://prometheus.io/docs/prometheus/latest/querying/api/#instant-vectors)
#'
#' @export
#'
#' @examplesIf FALSE
#' current_size <- query_prometheus_instant(
#'   query = "max(dirsize_total_size_bytes) by (directory, namespace)"
#' )
query_prometheus_instant <- function(
  grafana_url = "https://grafana.openscapes.2i2c.cloud",
  grafana_token = Sys.getenv("GRAFANA_TOKEN"),
  query,
  time = Sys.time()
) {
  prometheus_uid <- get_default_prometheus_uid(grafana_url, grafana_token)
  resp <- httr2::request(grafana_url) |>
    httr2::req_url_path(
      "/api/datasources/proxy/uid",
      prometheus_uid,
      "api/v1/query"
    ) |>
    httr2::req_options(http_version = 2) |>
    httr2::req_auth_bearer_token(grafana_token) |>
    httr2::req_url_query(query = query, time = time_string(time)) |>
    httr2::req_perform() |>
    httr2::resp_check_status()

  ret <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  as.prom_instant(ret)
}

#' Query prometheus for a range of dates
#'
#' Adapted from https://hackmd.io/NllqOUfaTLCXcDQPipr4rg
#'
#' @param grafana_url URL of the Grafana instance. Default
#'    `""https://grafana.openscapes.2i2c.cloud""`
#' @param grafana_token Authentication token for Grafana. By default reads from
#'    the environment variable `GRAFANA_TOKEN`
#' @param query Query in "PromQL"
#'   ([Prometheus Query Language](https://prometheus.io/docs/prometheus/latest/querying/basics/))
#' @param start_time Start of time range to query. Date or date-time object, or
#'    character of the form "YYYY-MM-DD HH:MM:SS". Time components are optional.
#'    Default is `end_time` - 30 days. If a `POSIXt` object, it will be converted
#'    to UTC, if a `Date` or `character` object, it will be assumed to be `UTC`
#' @param end_time End of time range to query. Date or date-time object, or
#'    character of the form "YYYY-MM-DD HH:MM:SS". Time components are optional.
#'    Default is today (`Sys.Date()`). If a `POSIXt` object, it will be converted
#'    to UTC, if a `Date` or `character` object, it will be assumed to be `UTC`
#' @param step Time step in seconds, or a string formatted as `"*h*m*s"` Eg., 1
#'    day would be `"24h0m0s"`.
#'
#' @return List containing the response from Prometheus, in the
#'    [range vector format](https://prometheus.io/docs/prometheus/latest/querying/api/#range-vectors)
#' @export
#'
#' @examplesIf FALSE
#' query_prometheus_range(
#'   query = "max(dirsize_total_size_bytes) by (directory, namespace)",
#'   start_time = "2024-01-01",
#'   end_time = "2024-05-28",
#'   step = 60 * 60 * 24
#' )
query_prometheus_range <- function(
  grafana_url = "https://grafana.openscapes.2i2c.cloud",
  grafana_token = Sys.getenv("GRAFANA_TOKEN"),
  query,
  start_time = end_time - 30,
  end_time = Sys.Date(),
  step
) {
  prometheus_uid <- get_default_prometheus_uid(grafana_url, grafana_token)
  req <- httr2::request(grafana_url) |>
    # Force HTTP version 2, I think there was a mismatch when not set and was
    # getting the error:
    #   Failed to perform HTTP request.
    #   Caused by error in `curl::curl_fetch_memory()`:
    #   ! Failed writing received data to disk/application.
    # Use `curl --i https://grafana.openscapes.2i2c.cloud/api/` on
    # command line to get supported HTTP version of server (it shows HTTP/2)
    # See curl::curl_symbols("http_version") for http version values
    httr2::req_options(http_version = 2) |>
    httr2::req_url_path(
      "/api/datasources/proxy/uid",
      prometheus_uid,
      "api/v1/query_range"
    ) |>
    httr2::req_auth_bearer_token(grafana_token) |>
    httr2::req_url_query(
      query = query,
      start = time_string(start_time),
      end = time_string(end_time),
      step = step
    )

  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_check_status()

  ret <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  as.prom_range(ret)
}

#' Create a data frame from a prometheus range query result
#'
#' @param x A `prom_result` object; the result of running [query_prometheus_range()] or [query_prometheus_instant()]
#' @param value_name A single string specifying the name for the value column.
#' @param value_fn A function to transform the value. Default [as.numeric()]
#'
#' @returns
#' A data frame with columns for metrics, a UTC datetime column named 'date',
#' and a numeric value column named according to `value_name`.
#'
#' @examplesIf FALSE
#'
#' range_res <- query_prometheus_range(
#'   query = "max(dirsize_total_size_bytes) by (directory, namespace)",
#'   start_time = "2024-01-01",
#'   end_time = "2024-05-28",
#'   step = 60 * 60 * 24
#' )
#' format_prom_result(range_res, "size (bytes)")
#' @export
format_prom_result <- function(x, value_name, value_fn = base::as.numeric) {
  UseMethod("format_prom_result")
}

#' @export
format_prom_result.default <- function(
  x,
  value_name,
  value_fn = base::as.numeric
) {
  cli::cli_abort("Unsupported type for {.fun format_prom_result}")
}

#' @export
format_prom_result.prom_range <- function(
  x,
  value_name,
  value_fn = base::as.numeric
) {
  metrics <- as.data.frame(x$data$result$metric)
  vals <- x$data$result$values

  out_df <- lapply(seq_along(vals), \(x) {
    vals <- as.data.frame(vals[[x]])
    cbind(metrics[x, , drop = FALSE], vals, row.names = NULL)
  }) |>
    purrr::list_rbind()

  out_df |>
    format_prom_df(value_name = value_name)
}

#' @export
format_prom_result.prom_instant <- function(
  x,
  value_name,
  value_fn = base::as.numeric
) {
  metrics <- as.data.frame(x$data$result$metric)
  vals <- as.data.frame(do.call(rbind, x$data$result$value))

  cbind(metrics, vals) |>
    format_prom_df(value_name = value_name, value_fn = value_fn)
}

format_prom_df <- function(x, value_name, value_fn = base::as.numeric) {
  if (!"V1" %in% names(x)) {
    cli::cli_abort("Missing date column")
  }

  if (!"V2" %in% names(x)) {
    cli::cli_abort("Missing value column")
  }

  x <- x |>
    dplyr::rename(
      date = "V1",
      "{value_name}" := "V2"
    )

  dplyr::mutate(
    x,
    date = prom_date(date),
    "{value_name}" := value_fn(.data[[value_name]])
  )
}

as.prom_range <- function(x) {
  structure(x, class = c("prom_range", "prom_result"))
}

as.prom_instant <- function(x) {
  structure(x, class = c("prom_instant", "prom_result"))
}
