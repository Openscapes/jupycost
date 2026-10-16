#' Get AWS credits used per month
#'
#' Queries AWS Cost Explorer for the credits applied to the account each
#' month. Cost Explorer only goes back about 13 months, and does not report
#' the remaining credit balance.
#'
#' @inheritParams get_daily_usage_costs
#' @param months_back A single integer <= 13 specifying how many months back to
#'   query. The query starts on the first day of the month `months_back` months
#'   before `end_date`.
#'
#' @returns
#' A data frame with a `month` column (the first day of each month) and a
#' `credits_used` column (in USD, positive). The last month is incomplete if
#' `end_date` is not the first of a month.
#'
#' @examples
#' \dontrun{
#' get_credits_used()
#' }
#'
#' @export
get_credits_used <- function(end_date = Sys.Date(), months_back = 12) {
  end_date <- check_valid_date(end_date)

  if (!rlang::is_integerish(months_back) || months_back > 13) {
    cli::cli_abort("{.arg months_back} must be an integer <= 13.")
  }

  start_date <- lubridate::floor_date(
    lubridate::add_with_rollback(end_date, -months(months_back)),
    unit = "month"
  )

  raw <- ce_credits_raw(
    format(start_date, "%Y-%m-%d"),
    format(end_date, "%Y-%m-%d")
  )

  dplyr::tibble(
    month = lubridate::ymd(purrr::map_chr(
      raw$ResultsByTime,
      \(x) x$TimePeriod$Start
    )),
    credits_used = -purrr::map_dbl(
      raw$ResultsByTime,
      \(x) as.double(x$Total$UnblendedCost$Amount)
    )
  )
}

ce_credits_raw <- function(start, end) {
  sixtyfour::con_ce()$get_cost_and_usage(
    TimePeriod = list(Start = start, End = end),
    Granularity = "MONTHLY",
    Metrics = list("UnblendedCost"),
    Filter = list(
      Dimensions = list(Key = "RECORD_TYPE", Values = list("Credit"))
    )
  )
}
