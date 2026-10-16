#' Get daily AWS usage costs
#'
#' @param end_date A Date object (or string in a standard unambiguous date
#'   format) representing the end of the desired date range. **Exclusive**:
#'   costs on `end_date` itself are not returned, matching the AWS Cost
#'   Explorer API. To include costs up to and including a given day, use the
#'   following day. The default, `Sys.Date()`, therefore excludes today's
#'   (incomplete) costs.
#' @param months_back Optional. A single integer specifying how many months
#'   back to query. The query starts on the first day of the month
#'   `months_back` months before `end_date`. Cannot be combined with
#'   `start_date`.
#' @param start_date Optional. A Date object (or string in a standard
#'   unambiguous date format) representing the first day of the desired date
#'   range. **Inclusive**: costs on `start_date` are returned. Use this to
#'   query a range that doesn't start on the first day of a month. If
#'   supplied, `months_back` must not be. Because `end_date` is exclusive,
#'   consecutive periods can be queried by using one period's `end_date` as
#'   the next period's `start_date`.
#' @param cost_type The type of costs. "unblended" (default), "blended", or "all"
#' @param hub which hub (or "all") you want costs for
#' @param cluster which cluster ("openscapeshub" or "nmfs-openscapes") you want information for. Ensure that you are authenticated to the correct AWS account using the appropriate API keys.
#'
#' @details
#' Costs are assigned to a `service_component` based on the AWS service, following 2i2c's cost
#' categories:
#'
#' * `"home storage"`: EBS volumes and snapshots tagged
#'   `2i2c:volume-purpose = home-nfs` (the `jupyterhub-home-nfs` home
#'   directory volumes).
#' * `"core"`: core node costs (instance hours and volumes tagged
#'   `2i2c:node-purpose = core`), the NAT gateway, hub database volumes, and
#'   support component (Prometheus, Grafana, Alertmanager) volumes.
#' * `"compute"`: the remainder, mostly user node instance hours and root
#'   volumes, plus any untagged snapshots or data transfer.
#'
#'
#' @returns
#' A data frame of AWS usage costs.
#'
#' @examples
#' \dontrun{
#' # Costs for a reporting period of Sep 22 to Oct 21, 2026 inclusive. The
#' # following period would start on 2026-10-22.
#' get_daily_usage_costs(start_date = "2026-09-22", end_date = "2026-10-22")
#'
#' # Costs for the full calendar months of June to September 2026
#' get_daily_usage_costs(end_date = "2026-10-01", months_back = 4)
#' }
#'
#' @export
get_daily_usage_costs <- function(
  end_date = Sys.Date(),
  months_back = 6,
  start_date = NULL,
  cost_type = c("unblended", "blended", "all"),
  hub = c("all", "prod", "staging", "workshop", "support"),
  cluster = c("openscapeshub", "nmfs-openscapes")
) {
  end_date <- check_valid_date(end_date)

  hub <- match.arg(hub)

  cluster <- match.arg(cluster)

  if (!rlang::is_integerish(months_back) || months_back > 12) {
    cli::cli_abort("{.arg months_back} must be an integer <= 12.")
  }

  cost_type <- match.arg(cost_type)

  if (is.null(start_date)) {
    start_date <- lubridate::floor_date(
      lubridate::add_with_rollback(end_date, -months(months_back)),
      unit = "month"
    )
  } else {
    if (!missing(months_back)) {
      cli::cli_abort(
        "Supply only one of {.arg start_date} or {.arg months_back}."
      )
    }
    start_date <- check_valid_date(start_date)
  }

  if (start_date >= end_date) {
    cli::cli_abort(
      "The resolved {.arg start_date} ({start_date}) must be before {.arg end_date} ({end_date})."
    )
  }

  # Daily Cost Explorer data only goes back about 13 months
  if (start_date < lubridate::add_with_rollback(end_date, -months(13))) {
    cli::cli_abort(
      "The resolved {.arg start_date} ({start_date}) is more than 13 months before {.arg end_date} ({end_date}). Daily Cost Explorer data is not available that far back."
    )
  }

  base_filters <- list(ce_filter_usage(), ce_filter_attributable_costs(cluster))
  hub_filter <- ce_filter_hub(hub)
  if (!is.null(hub_filter)) {
    base_filters <- c(base_filters, list(hub_filter))
  }

  query_billing <- function(...) {
    aws_billing_daily(
      format(start_date, "%Y-%m-%d"),
      format(end_date, "%Y-%m-%d"),
      filter = list(And = c(base_filters, list(...))),
      cost_type = cost_type
    )
  }

  daily_ce <- query_billing() |>
    dplyr::mutate(date = lubridate::ymd(.data$date))

  daily_ce$service_component <- unname(ce_service_map()[daily_ce$service])
  daily_ce$service_component[is.na(daily_ce$service_component)] <- "other"

  split_ec2_other <- "EC2 - Other" %in% daily_ce$service
  split_core_compute <- "Amazon Elastic Compute Cloud - Compute" %in%
    daily_ce$service

  if (!split_ec2_other && !split_core_compute) {
    return(daily_ce)
  }

  parts <- list()
  core_parts <- list()

  if (split_ec2_other) {
    parts[["home storage"]] <- query_billing(
      ce_filter_ec2_other(),
      ce_filter_home_storage()
    )

    # Exclude home storage so the two subsets of EC2 - Other can't overlap
    core_parts[["ec2_other"]] <- query_billing(
      ce_filter_ec2_other(),
      list(Not = ce_filter_home_storage()),
      ce_filter_core_ec2_other()
    )
  }

  if (split_core_compute) {
    core_parts[["ec2_compute"]] <- query_billing(
      ce_filter_ec2_compute(),
      ce_filter_core_node()
    )
  }

  parts[["core"]] <- dplyr::bind_rows(core_parts)

  split_service_components(daily_ce, parts)
}

#' Split service costs into service components
#'
#' @param daily_ce Data frame of daily costs with a `service_component` column.
#' @param parts Named list of billing data frames (as returned by
#'   `sixtyfour::aws_billing()`), each a tagged subset of the costs of one of
#'   the services in `daily_ce`. Names are used as the `service_component` for
#'   those costs.
#'
#' @returns `daily_ce` with each row of an affected service replaced by one row
#'   per component in `parts`, plus a remainder row keeping the original
#'   `service_component`.
#'
#' @noRd
split_service_components <- function(daily_ce, parts) {
  keys <- c("id", "date", "linked_account")
  out_cols <- names(daily_ce)

  parts <- Filter(\(x) NROW(x) > 0, parts)
  if (length(parts) == 0) {
    return(daily_ce)
  }

  part_costs <- dplyr::bind_rows(parts, .id = "service_component") |>
    dplyr::mutate(date = lubridate::ymd(.data$date)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(
      "service",
      keys,
      "service_component"
    )))) |>
    dplyr::summarise(part_cost = sum(.data$cost), .groups = "drop")

  daily_ce$.row <- seq_len(nrow(daily_ce))
  is_split <- daily_ce$service %in% unique(part_costs$service)
  to_split <- daily_ce[is_split, ]

  split_rows <- to_split |>
    dplyr::select(-"cost", -"service_component") |>
    dplyr::inner_join(part_costs, by = c("service", keys)) |>
    dplyr::rename(cost = "part_cost")

  part_totals <- part_costs |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("service", keys)))) |>
    dplyr::summarise(parts_total = sum(.data$part_cost), .groups = "drop")

  remainder_rows <- to_split |>
    dplyr::left_join(part_totals, by = c("service", keys)) |>
    dplyr::mutate(
      cost = .data$cost - dplyr::coalesce(.data$parts_total, 0)
    ) |>
    dplyr::select(-"parts_total")

  if (any(remainder_rows$cost < -0.01)) {
    cli::cli_warn(c(
      "Tagged costs exceed the total for their service on some days.",
      "i" = "The remainder {.val compute} component contains negative values.",
      "i" = "This can happen when Cost Explorer revises estimated costs for recent days between the queries in one call."
    ))
  }

  component_order <- c("compute", names(parts))

  dplyr::bind_rows(daily_ce[!is_split, ], remainder_rows, split_rows) |>
    dplyr::arrange(
      .data$.row,
      match(.data$service_component, component_order)
    ) |>
    dplyr::select(dplyr::all_of(out_cols))
}
