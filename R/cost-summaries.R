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
  cost_type = c("unblended", "blended", "all")
) {
  end_date <- check_valid_date(end_date)

  if (!rlang::is_integerish(months_back) || months_back > 12) {
    cli::cli_abort("{.arg months_back} must be an integer <= 12.")
  }

  cost_type <- match.arg(cost_type)

  start_date <- lubridate::floor_date(
    lubridate::add_with_rollback(end_date, -months(months_back)),
    unit = "month"
  )

  raw_daily <- sixtyfour::aws_billing(
    as.character(start_date),
    as.character(end_date),
    filter = list(
      Dimensions = list(
        Key = "RECORD_TYPE",
        Values = "Usage"
      )
    )
  )

  if (cost_type != "all") {
    raw_daily <- dplyr::filter(raw_daily, id == cost_type)
  }

  raw_daily |>
    dplyr::mutate(date = lubridate::ymd(.data$date))
}
