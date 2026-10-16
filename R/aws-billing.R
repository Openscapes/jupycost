#' Query daily AWS costs by service and linked account
#'
#' Like `sixtyfour::aws_billing()`, but only sends the Cost Explorer requests
#' needed for `cost_type`, rather than always requesting both unblended and
#' blended costs.
#'
#' @param date_start,date_end Start (inclusive) and end (exclusive) dates as
#'   `"YYYY-MM-DD"` strings.
#' @param filter A Cost Explorer filter expression.
#' @param cost_type One of `"unblended"`, `"blended"` or `"all"`.
#'
#' @returns A data frame with the same columns as `sixtyfour::aws_billing()`.
#'
#' @noRd
aws_billing_daily <- function(
  date_start,
  date_end,
  filter = NULL,
  cost_type = c("unblended", "blended", "all")
) {
  cost_type <- match.arg(cost_type)
  metrics <- c(unblended = "UnblendedCost", blended = "BlendedCost")
  if (cost_type != "all") {
    metrics <- metrics[cost_type]
  }

  purrr::imap(
    metrics,
    \(metric, id) {
      aws_billing_one(date_start, date_end, filter, metric) |>
        dplyr::mutate(id = id, .before = 1)
    }
  ) |>
    purrr::list_rbind()
}

aws_billing_one <- function(date_start, date_end, filter, metric) {
  raw <- aws_billing_raw(
    date_start,
    metrics = metric,
    granularity = "daily",
    group_by = list(
      list(Type = "DIMENSION", Key = "SERVICE"),
      list(Type = "DIMENSION", Key = "LINKED_ACCOUNT")
    ),
    date_end = date_end,
    filter = filter
  )

  if (length(raw$NextPageToken) > 0 && nzchar(raw$NextPageToken)) {
    cli::cli_abort(c(
      "The Cost Explorer response was paginated, so some costs would be missing.",
      "i" = "Try a shorter date range."
    ))
  }

  empty <- dplyr::tibble(
    date = character(),
    service = character(),
    linked_account = character(),
    cost = double()
  )

  purrr::map(raw$ResultsByTime, \(x) {
    dplyr::tibble(
      date = x$TimePeriod$Start,
      service = purrr::map_chr(x$Groups, \(g) g$Keys[[1]]),
      linked_account = purrr::map_chr(x$Groups, \(g) g$Keys[[2]]),
      cost = purrr::map_dbl(x$Groups, \(g) {
        as.double(g$Metrics[[metric]]$Amount)
      })
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(empty) |>
    dplyr::left_join(sixtyfour::service_map, by = "service") |>
    dplyr::mutate(acronym = dplyr::coalesce(.data$acronym, .data$service))
}

# Mockable wrapper, so tests don't need to modify the sixtyfour namespace
aws_billing_raw <- function(...) {
  sixtyfour::aws_billing_raw(...)
}
