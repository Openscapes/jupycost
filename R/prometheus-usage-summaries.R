#' Get daily users
#'
#' @inheritParams query_prometheus_range
#' @param step Time step in days (default `1`).
#' @param aggregation time period over which to aggregate, in days (integer,
#'   default `1`).
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

  create_range_df(res, "n_users") |>
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
  create_range_df(res, "n_users")
}
