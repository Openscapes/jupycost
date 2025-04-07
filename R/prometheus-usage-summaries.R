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
#' @inheritParams query_prometheus_instant
#'
#' @returns
#' A data frame of directory information:
#' - `namespace`: Hub Namespace (prod, staging, workshop)
#' - `directory`: User directory
#' - `last_accessed`: Date of last access
#' - `dirsize_mb`: Size of directory in MB
#' - `n_files`: Number of files
#' - `percent_total_size`: Percentage of total directory size
#'
#' @export
user_dir_snapshot <- function(
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
  ) |>
    format_prom_result(
      value_name = "dirsize_mb",
      value_fn = \(x) as.numeric(x) * 1e-6
    )

  n_files <- query_prometheus_instant(
    grafana_url = grafana_url,
    grafana_token = grafana_token,
    query = "max(dirsize_entries_count) by (namespace, directory)",
    time = time
  ) |>
    format_prom_result(
      value_name = "n_files"
    )

  join_cols <- c("namespace", "directory", "date")

  last_accessed |>
    dplyr::left_join(size, by = join_cols) |>
    dplyr::left_join(n_files, by = join_cols) |>
    dplyr::mutate(
      directory = unsanitize_dir_names(.data$directory),
      percent_total_size = .data$dirsize_mb / sum(.data$dirsize_mb) * 100,
      .by = "namespace"
    ) |>
    dplyr::select(
      "date",
      "namespace",
      "directory",
      "last_accessed",
      "n_files",
      "dirsize_mb",
      "percent_total_size"
    )
}

#' Query directory sizes over time from Grafana
#'
#' @param by_user A logical value indicating whether to group by user (directory). Defau
#' @inheritParams query_prometheus_range
#'
#' @returns
#' A data frame of directory sizes over time, with dirctory sizes in megabytes.
#'
#' @export
dir_sizes <- function(
  grafana_url = "https://grafana.openscapes.2i2c.cloud",
  grafana_token = Sys.getenv("GRAFANA_TOKEN"),
  start_time = end_time - 30,
  end_time = Sys.Date(),
  by_user = FALSE,
  step = "1h0m0s"
) {
  # dirsize_total_size_bytes is a metric calculated at the root user directory
  # level (it doesn't calculate for subdirectories;
  # https://github.com/yuvipanda/prometheus-dirsize-exporter/tree/main?tab=readme-ov-file#metrics-recorded),
  # so if grouping by directory or select a single user, sum() will be equal to
  # max(). But if not grouping by directory, if we want the total size of all
  # user directories, we need to sum().
  if (by_user) {
    query <- 'max(dirsize_total_size_bytes) by (namespace, directory)'
  } else {
    query <- 'sum(dirsize_total_size_bytes) by (namespace)'
  }

  ret <- query_prometheus_range(
    grafana_url = grafana_url,
    grafana_token = grafana_token,
    query = query,
    start_time = start_time,
    end_time = end_time,
    step = step
  ) |>
    format_prom_result(
      value_name = "dirsize_mb",
      value_fn = \(x) as.numeric(x) * 1e-6
    )

  if (by_user) {
    ret <- ret |>
      dplyr::mutate(
        directory = unsanitize_dir_names(.data$directory)
      )
  }

  ret
}
