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

#' Get AWS credit balances
#'
#' Queries the AWS Billing API for the credits on the account, including how
#' much of each credit remains.
#'
#' @param months_back A single integer <= 12 specifying how far back to look
#'   for credits, in months. The AWS API does not return credits that
#'   ended or were exhausted more than a year ago.
#'
#' @returns
#' A data frame with one row per credit and columns `credit_id`,
#' `description`, `initial_amount`, `remaining_amount`, `estimated_amount`,
#' `start_date`, `end_date` and `exhaust_date` (`NA` if not exhausted).
#' Amounts are in USD. `remaining_amount` is the last finalized balance, and
#' `estimated_amount` also accounts for usage so far in the current month.
#'
#' @examples
#' \dontrun{
#' get_credits_remaining()
#' }
#'
#' @export
get_credits_remaining <- function(months_back = 12) {
  if (!rlang::is_integerish(months_back) || months_back > 12) {
    cli::cli_abort("{.arg months_back} must be an integer <= 12.")
  }

  start_date <- lubridate::add_with_rollback(Sys.Date(), -months(months_back))
  start_date <- max(start_date, Sys.Date() - 364)

  amount <- \(x, field) {
    purrr::map_dbl(x, \(credit) as.double(credit[[field]]$currencyAmount))
  }
  time <- \(x, field) {
    secs <- purrr::map_dbl(x, \(credit) {
      if (length(credit[[field]]) == 0) {
        NA_real_
      } else {
        as.numeric(credit[[field]])
      }
    })
    as.POSIXct(secs, origin = "1970-01-01", tz = "UTC")
  }

  credits <- billing_credits_raw(start_date)$credits

  dplyr::tibble(
    credit_id = purrr::map_chr(credits, "creditId"),
    description = purrr::map_chr(credits, "description"),
    initial_amount = amount(credits, "initialAmount"),
    remaining_amount = amount(credits, "remainingAmount"),
    estimated_amount = amount(credits, "estimatedAmount"),
    start_date = time(credits, "startDate"),
    end_date = time(credits, "endDate"),
    exhaust_date = time(credits, "exhaustDate")
  )
}

# The endpoint is set explicitly because paws.cost.management builds a
# hostname that doesn't exist
billing_credits_raw <- function(start_date) {
  account_id <- paws.security.identity::sts()$get_caller_identity()$Account
  paws.cost.management::billing(
    config = list(
      region = "us-east-1",
      endpoint = "https://billing.us-east-1.api.aws"
    )
  )$get_credits(accountId = account_id, startDate = start_date)
}
