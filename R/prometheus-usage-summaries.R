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
    query = glue_promql(
      'count(
        sum(
          min_over_time(
            kube_pod_labels{
              label_app="jupyterhub",
              label_component="singleuser-server",
              label_hub_jupyter_org_username!~"(service|perf|hubtraf)-",
          }[<aggregation>d]
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
      date_time = tidyr::full_seq(.data$date_time, parse_step_to_seconds(step)),
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

#' Get workshop user count over a time period
#'
#' @description
#' Count distinct user directories that existed in the workshop namespace over a
#' given time period. This includes users whose directories have
#' since been removed: Prometheus preserves historical `dirsize_total_size_bytes`
#' time series even after a home directory is deleted, so any directory that had
#' at least one sample within the queried window is counted. Typically the data
#' is retained in prometheus for 3 years, so this should be able to look back
#' that far.
#'
#' @inheritParams query_prometheus_range
#' @param namespace Hub namespace to filter on. Default `"workshop"`. Set to
#'   `NULL` to query all namespaces.
#' @param exclude_pattern An optional regular expression matched against
#'   directory names to exclude (e.g., admin or infrastructure accounts).
#'   Default `NULL` (no exclusions).
#' @param by_user If `TRUE`, return one row per directory with approximate
#'   `first_seen` and `last_seen` dates instead of a summary count.
#'   Default `FALSE`.
#' @param step Resolution for `first_seen`/`last_seen` timestamps when
#'   `by_user = TRUE`, as a string in `"*h*m*s"` format (e.g. `"24h0m0s"` for
#'   daily resolution). Finer steps improve timestamp accuracy at the cost of a
#'   more expensive query. Ignored when `by_user = FALSE`. Default 1 day (`"24h0m0s"`).
#'
#' @returns
#' When `by_user = FALSE` (default): a data frame with one row per namespace
#' and columns `namespace`, `n_users`, `start_time`, `end_time`.
#'
#' When `by_user = TRUE`: a data frame with one row per user directory and
#' columns `namespace`, `directory`, `first_seen`, `last_seen`, `start_time`,
#' `end_time`. Timestamps are approximate to the resolution of `step`.
#' A `last_seen` close to `end_time` indicates the directory likely still
#' exists; a `last_seen` well before `end_time` indicates deletion.
#'
#' @export
get_workshop_users <- function(
  grafana_url = "https://grafana.openscapes.2i2c.cloud",
  grafana_token = Sys.getenv("GRAFANA_TOKEN"),
  start_time = end_time - 30,
  end_time = Sys.Date(),
  namespace = "workshop",
  exclude_pattern = NULL,
  by_user = FALSE,
  step = "24h0m0s"
) {
  # Compute the duration in seconds between start and end. This becomes the
  # range vector lookback passed to max_over_time(), ensuring we capture
  # directories that existed at any point in the window, including deleted ones.
  duration_secs <- as.integer(
    difftime(
      lubridate::as_datetime(end_time, tz = "UTC"),
      lubridate::as_datetime(start_time, tz = "UTC"),
      units = "secs"
    )
  )
  if (is.na(duration_secs) || duration_secs <= 0) {
    cli::cli_abort("{.arg start_time} must be earlier than {.arg end_time}.")
  }
  # Build PromQL label selectors
  selectors <- if (!is.null(namespace)) {
    paste0('namespace="', namespace, '"')
  } else {
    'namespace=~".*"'
  }
  if (!is.null(exclude_pattern)) {
    selectors <- paste0(selectors, ', directory!~"', exclude_pattern, '"')
  }

  if (by_user) {
    step_secs <- parse_step_to_seconds(step)

    # max(...) by (namespace, directory) collapses any extra label dimensions
    # (e.g. node/instance) so each directory appears exactly once.
    dir_query <- glue_promql(
      'max(
        max_over_time(
          dirsize_total_size_bytes{<selectors>}[<duration_secs>s]
        )
      ) by (namespace, directory)'
    )

    first_seen_query <-
      raw_dirs <- query_prometheus_instant(
        grafana_url = grafana_url,
        grafana_token = grafana_token,
        query = dir_query,
        time = end_time
      )

    if (length(raw_dirs$data$result) == 0) {
      return(
        data.frame(
          namespace = character(),
          directory = character(),
          first_seen = as.Date(character()),
          last_seen = as.Date(character()),
          start_time = as.Date(character()),
          end_time = as.Date(character())
        )
      )
    }

    # PromQL subqueries: evaluate timestamp() at every step_secs over the full
    # window, then take min/max. Gives the first/last scrape at which each
    # directory was observed — a proxy for creation and deletion time.
    seen_query <- 'max(
        <when>_over_time(
          timestamp(
            dirsize_total_size_bytes{<selectors>}
          )[<duration_secs>s:<step_secs>s]
        )
      ) by (namespace, directory)'

    raw_first <- query_prometheus_instant(
      grafana_url = grafana_url,
      grafana_token = grafana_token,
      query = glue_promql(seen_query, when = "min"),
      time = end_time
    )

    raw_last <- query_prometheus_instant(
      grafana_url = grafana_url,
      grafana_token = grafana_token,
      query = glue_promql(seen_query, when = "max"),
      time = end_time
    )

    dirs <- format_prom_result(
      raw_dirs,
      value_name = "dirsize_mb",
      value_fn = \(x) as.numeric(x) * 1e-6
    )

    first_seen <- format_prom_result(
      raw_first,
      "first_seen",
      value_fn = prom_date
    )

    last_seen <- format_prom_result(
      raw_last,
      "last_seen",
      value_fn = prom_date
    )

    # Join on sanitized names so keys are stable, then unsanitize afterwards.
    join_cols <- c("namespace", "directory")

    dirs |>
      dplyr::left_join(
        dplyr::select(first_seen, "namespace", "directory", "first_seen"),
        by = join_cols
      ) |>
      dplyr::left_join(
        dplyr::select(last_seen, "namespace", "directory", "last_seen"),
        by = join_cols
      ) |>
      dplyr::mutate(
        directory = unsanitize_dir_names(.data$directory),
        first_seen = as.Date(.data$first_seen),
        last_seen = as.Date(.data$last_seen),
        start_time = as.Date(start_time),
        end_time = as.Date(end_time)
      ) |>
      dplyr::select(
        "namespace",
        "directory",
        "first_seen",
        "last_seen",
        "start_time",
        "end_time"
      ) |>
      dplyr::distinct()
  } else {
    # The intermediate max(...) by (namespace, directory) collapses extra label
    # dimensions before count(), ensuring we count distinct directories only.
    query <- glue_promql(
      'count(
        max(
          max_over_time(
            dirsize_total_size_bytes{<selectors>}[<duration_secs>s]
          )
        ) by (namespace, directory)
      ) by (namespace)'
    )

    raw <- query_prometheus_instant(
      grafana_url = grafana_url,
      grafana_token = grafana_token,
      query = query,
      time = end_time
    )

    if (length(raw$data$result) == 0) {
      return(
        data.frame(
          namespace = character(),
          n_users = integer(),
          start_time = as.Date(character()),
          end_time = as.Date(character())
        )
      )
    }

    format_prom_result(raw, value_name = "n_users", value_fn = as.integer) |>
      dplyr::mutate(
        start_time = as.Date(start_time),
        end_time = as.Date(end_time)
      ) |>
      dplyr::select("namespace", "n_users", "start_time", "end_time")
  }
}

#' Query directory sizes over time from Grafana
#'
#' @param by_user A logical value indicating whether to group by user (directory). Default FALSE
#' @inheritParams query_prometheus_range
#'
#' @returns
#' A data frame of directory sizes over time, with directory sizes in megabytes.
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


#' Query user memory requests from Grafana
#'
#' @description
#' Query user memory requests from Grafana. This gives the  memory requests
#' by a user, in addition to the instance type and container image they are using,
#' by specified time step for a given time range.
#'
#' @inheritParams query_prometheus_range
#'
#' @returns
#' User memory request data.
#'
#' @export
user_mem_requests <- function(
  grafana_url = "https://grafana.openscapes.2i2c.cloud",
  grafana_token = Sys.getenv("GRAFANA_TOKEN"),
  start_time = end_time - 30,
  end_time = Sys.Date(),
  step = "0h10m0s"
) {
  ret <- query_prometheus_range(
    grafana_url = grafana_url,
    grafana_token = grafana_token,
    query = resource_requests_query("memory"),
    start_time = start_time,
    end_time = end_time,
    step = step
  ) |>
    format_prom_result(
      value_name = "mem_mb",
      value_fn = \(x) as.numeric(x) * 1e-6
    )

  ret
}

#' Query user CPU requests from Grafana
#'
#' @description
#' Query user cpu requests from Grafana. This gives the cpu requests
#' by a user, in addition to the instance type and container image they are using,
#' by specified time step for a given time range.
#'
#' @inheritParams query_prometheus_range
#'
#' @returns
#' User cpu request data.
#'
#' @export
user_cpu_requests <- function(
  grafana_url = "https://grafana.openscapes.2i2c.cloud",
  grafana_token = Sys.getenv("GRAFANA_TOKEN"),
  start_time = end_time - 30,
  end_time = Sys.Date(),
  step = "0h10m0s"
) {
  ret <- query_prometheus_range(
    grafana_url = grafana_url,
    grafana_token = grafana_token,
    query = resource_requests_query("cpu"),
    start_time = start_time,
    end_time = end_time,
    step = step
  ) |>
    format_prom_result(
      value_name = "cpu_cores",
      value_fn = \(x) as.numeric(x)
    )

  ret
}

resource_requests_query <- function(resource) {
  glue_promql(
    'sum(
  kube_pod_container_resource_requests{resource="<resource>", pod=~"jupyter-.*"}
  * on(node) group_left(label_beta_kubernetes_io_instance_type)
  kube_node_labels
) by (namespace, pod, label_beta_kubernetes_io_instance_type, node)
* on(namespace, pod) group_left(image_id)
kube_pod_container_info{namespace=~".*", pod=~"jupyter-.*"}'
  )
}

# Resource allocation is set here:
# https://github.com/2i2c-org/infrastructure/blob/bf1225f89162e525f58caa537b6181c27d9c941e/config/clusters/openscapes/common.values.yaml#L106-L172.
# AFAICT the mem_limit and mem_guarantee essentially dictate how many pods can fit in a node. cpu_limit is the upper cpu resources a user will get,
# depending on how many pods are running on a node, and how cpu intensive the workloads are.
# If you choose a Resource Allocation that has the highest memory for the CPU, then you will get a node to yourself...

#' Query user memory usage from Grafana
#'
#' @description
#' Query user memory usage from Grafana. This gives the actual memory usage (in MB)
#' by a user, in addition to the user pod and hub namespace,
#' by specified time step for a given time range.
#'
#' @inheritParams query_prometheus_range
#'
#' @returns
#' A data frame containing pod memory usage.
#'
#' @export
user_mem_usage <- function(
  grafana_url = "https://grafana.openscapes.2i2c.cloud",
  grafana_token = Sys.getenv("GRAFANA_TOKEN"),
  start_time = end_time - 30,
  end_time = Sys.Date(),
  step = "0h10m0s"
) {
  ret <- query_prometheus_range(
    grafana_url = grafana_url,
    grafana_token = grafana_token,
    query = 'sum(
  # exclude name="" because the same container can be reported
  # with both no name and `name=k8s_...`,
  # in which case sum() by (pod) reports double the actual metric
  container_memory_working_set_bytes{name!="", instance=~".*"}
  * on (namespace, pod) group_left(container)
  group(
      kube_pod_labels{label_app="jupyterhub", label_component="singleuser-server", namespace=~".*", pod=~".*"}
  ) by (pod, namespace)
) by (pod, namespace)',
    start_time = start_time,
    end_time = end_time,
    step = step
  )

  res <- ret |>
    format_prom_result(
      value_name = "mem_mb",
      value_fn = \(x) as.numeric(x) * 1e-6
    )

  res
}

#' Query user CPU usage from Grafana
#'
#' @description
#' Query user cpu usage from Grafana. This gives the actual cpu usage (in percentage)
#' by a user, in addition to the user pod and hub namespace,
#' by specified time step for a given time range.
#'
#' @inheritParams query_prometheus_range
#'
#' @returns
#' A data frame containing user CPU usage.
#'
#' @export
user_cpu_usage <- function(
  grafana_url = "https://grafana.openscapes.2i2c.cloud",
  grafana_token = Sys.getenv("GRAFANA_TOKEN"),
  start_time = end_time - 30,
  end_time = Sys.Date(),
  step = "0h10m0s"
) {
  ret <- query_prometheus_range(
    grafana_url = grafana_url,
    grafana_token = grafana_token,
    query = 'sum(
  # exclude name="" because the same container can be reported
  # with both no name and `name=k8s_...`,
  # in which case sum() by (pod) reports double the actual metric
  irate(container_cpu_usage_seconds_total{name!="", instance=~".*"}[5m])
  * on (namespace, pod) group_left(container)
  group(
      kube_pod_labels{label_app="jupyterhub", label_component="singleuser-server", namespace=~".*", pod=~".*"}
  ) by (pod, namespace)
) by (pod, namespace)',
    start_time = start_time,
    end_time = end_time,
    step = step
  )

  res <- ret |>
    format_prom_result(
      value_name = "cpu_percent",
      value_fn = \(x) as.numeric(x) * 1e-6
    )

  res
}


resource_usage_query <- function(resource) {
  sum_line <- switch(
    resource,
    "cpu" = 'irate(container_cpu_usage_seconds_total{name!="", instance=~".*", pod!="jupyter-deployment-service-check",pod=~"jupyter-.*"}[5m])',
    "memory" = 'container_memory_working_set_bytes{name!="", instance=~".*", pod!="jupyter-deployment-service-check",pod=~"jupyter-.*"}' # "container_memory_usage_bytes" includes cache which may be misleading
  )

  paste0(
    'sum(
  # exclude name="" because the same container can be reported
  # with both no name and `name=k8s_...`,
  # in which case sum() by (pod) reports double the actual metric
  # TODO: Not irate for memory!',
    sum_line,
    '* on (namespace, pod) group_left(annotation_hub_jupyter_org_username)
  group(
      kube_pod_annotations{namespace=~".*", annotation_hub_jupyter_org_username=~".*"}
  ) by (pod, namespace, annotation_hub_jupyter_org_username)
) by (annotation_hub_jupyter_org_username, namespace)'
  )
}
