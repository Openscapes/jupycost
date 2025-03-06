#' Get daily user counts
#'
#' @inheritParams query_prometheus_range
#' @param aggregation time period over which to aggregate, in days (integer,
#'   default `1`).
#' @param step Time step in days (default `1`).
#' @inheritParams query_prometheus_range
#'
#' @returns
#' A data frame with daily user counts, grouped by namespace.
#'
#' @export
get_daily_users <- function(
  grafana_url = "https://grafana.openscapes.2i2c.cloud",
  grafana_token = Sys.getenv("GRAFANA_TOKEN"),
  start_time = end_time - 30,
  end_time = Sys.Date(),
  aggregation = 1,
  step = 1
) {
  res <- query_prometheus_range(
    grafana_url = grafana_url,
    grafana_token = grafana_token,
    query = glue::glue(
      'count(
        sum(
          min_over_time(
            kube_pod_labels{{
              label_app="jupyterhub",
              label_component="singleuser-server",
              label_hub_jupyter_org_username!~"(service|perf|hubtraf)-",
          }}[{aggregation}d]
          )
        ) by (pod, namespace)
      ) by (namespace)'
    ),
    start_time = start_time,
    end_time = end_time,
    step = glue::glue(step * 24, "h0m0s")
  )

  format_prom_result(res, "n_users") |>
    dplyr::mutate(date = as.Date(date)) |>
    # Fill in zeros for missing dates
    tidyr::complete(
      date = tidyr::full_seq(.data$date, 1),
      .data$namespace,
      fill = list(n_users = 0)
    )
}

#' Get hourly user counts
#'
#' @inheritParams query_prometheus_range
#' @param step Time step in seconds, or a string formatted as `"*h*m*s"`
#'   Eg., Default 1 hour: `"1h0m0s"`.
#'
#' @returns
#' A dataframe of hourly user counts, grouped by namespace.
#'
#' @export
get_hourly_users <- function(
  grafana_url = "https://grafana.openscapes.2i2c.cloud",
  grafana_token = Sys.getenv("GRAFANA_TOKEN"),
  start_time = end_time - 30,
  end_time = Sys.Date(),
  step = "1h0m0s"
) {
  res <- query_prometheus_range(
    grafana_url = grafana_url,
    grafana_token = grafana_token,
    query = 'sum(
        kube_pod_status_phase{phase="Running"}
        * on(pod, namespace) 
        kube_pod_labels{label_app="jupyterhub", label_component="singleuser-server"}
      ) by (namespace)',
    start_time = start_time,
    end_time = end_time,
    step = step
  )
  format_prom_result(res, "n_users") |>
    dplyr::rename(date_time = date) |>
    # Fill in zeros for missing dates
    tidyr::complete(
      date_time = tidyr::full_seq(.data$date_time, 1),
      .data$namespace,
      fill = list(n_users = 0)
    )
}

#' Get user directory information
#'
#' @param grafana_url Optional. A single string specifying the Grafana URL.
#' @param grafana_token Optional. A single string containing the Grafana API token.
#' @inheritParams query_prometheus_instant
#'
#' @returns
#' A data frame of directory information. Will error if the API token is not set
#' or if the API request fails.
#'
#' @export
user_dir_info <- function(
  grafana_url = "https://grafana.openscapes.2i2c.cloud",
  grafana_token = Sys.getenv("GRAFANA_TOKEN"),
  time = Sys.time()
) {
  last_accessed <- query_prometheus_instant(
    grafana_url = grafana_url,
    grafana_token = grafana_token,
    query = "min(dirsize_latest_mtime) by (namespace, directory)",
    time = time
  ) |>
    format_prom_result(
      value_name = "last_accessed",
      value_fn = prom_date
    )

  size <- query_prometheus_instant(
    grafana_url = grafana_url,
    grafana_token = grafana_token,
    query = "max(dirsize_total_size_bytes) by (namespace, directory)",
    time = time
  )

  size <- size |>
    format_prom_result(
      value_name = "dirsize_mb",
      value_fn = \(x) as.numeric(x) * 1e-6
    )

  last_accessed |>
    dplyr::left_join(size, by = c("namespace", "directory", "date"))
}
