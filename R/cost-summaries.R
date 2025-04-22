#' Get daily AWS usage costs
#'
#' @param end_date A Date object representing the end of the desired date range.
#' @param months_back Optional. A single integer specifying how many months back to query.
#' @param cost_type The type of costs. "unblended" (default), "blended", or "all"
#'
#' @returns
#' A data frame of AWS usage costs.
#'
#' @export
get_daily_usage_costs <- function(
  end_date = Sys.Date(),
  months_back = 6,
  cost_type = c("unblended", "blended", "all"),
  hub = c("all", "prod", "staging", "workshop", "shared"),
  cluster = c("openscapes", "nmfs-openscapes")
) {
  end_date <- check_valid_date(end_date)

  hub <- match.arg(hub)

  cluster = match.arg(cluster)

  if (!rlang::is_integerish(months_back) || months_back > 12) {
    cli::cli_abort("{.arg months_back} must be an integer <= 12.")
  }

  cost_type <- match.arg(cost_type)

  start_date <- lubridate::floor_date(
    lubridate::add_with_rollback(end_date, -months(months_back)),
    unit = "month"
  )

  filter_list <- list(
    And = list(
      Dimensions = list(
        Key = "RECORD_TYPE",
        Values = list("Usage")
      ),
      ce_filter_attributable_costs(cluster)
    )
  )

  if (hub == "shared") {
    filter_list = list(
      And = list(
        filter_list[["And"]],
        list(
          Tags = list(
            Key = "2i2c:hub-name",
            MatchOptions = list("ABSENT")
          )
        )
      )
    )
  } else if (hub != "all") {
    filter_list = list(
      And = list(
        filter_list[["And"]],
        list(
          Tags = list(
            Key = "2i2c:hub-name",
            Values = list(hub),
            MatchOptions = list("EQUALS")
          )
        )
      )
    )
  }

  raw_daily <- sixtyfour::aws_billing(
    as.character(start_date),
    as.character(end_date),
    filter = filter_list
  )

  if (cost_type != "all") {
    raw_daily <- dplyr::filter(raw_daily, .data$id == cost_type)
  }

  daily_ce <- raw_daily |>
    dplyr::mutate(date = lubridate::ymd(.data$date))

  daily_ce$service_component <- ce_service_map()[daily_ce$service]
  daily_ce$service_component[is.na(daily_ce$service_component)] <- "other"
  daily_ce
}
