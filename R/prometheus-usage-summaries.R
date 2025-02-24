#' Get daily users
#'
#' @inheritParams query_prometheus_range
#' @param step Time step in seconds, or a string formatted as `"*h*m*s"`
#'   Default 1 day: `"24h0m0s"`.
#' @param aggregation time period over which to aggregate, in days (integer,
#'   default `1`).
#'
#' @returns
#' A data frame with daily user counts, grouped by namespace.
#'
#' @export
get_daily_users <- function(
  start_time = end_time - 30,
  end_time = Sys.Date(),
  aggregation = 1,
  step = aggregation * 24
) {
  res <- query_prometheus_range(
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
    step = glue::glue(step, "h0m0s")
  )

  create_range_df(res, "n_users")
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
  start_time = end_time - 30,
  end_time = Sys.Date(),
  step = "1h0m0s"
) {
  res <- query_prometheus_range(
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
