#' Convert Cost Explorer result to a data frame
#'
#' @param ce_result A Cost Explorer API result object
#' @param metric One of the AWS Cost Explorer metrics. Optional; defaults to `"UnblendedCost"`.
#'
#' @returns
#' A data frame containing the Cost Explorer results.
#'
#' @export
ce_to_df <- function(ce_result, metric = "UnblendedCost") {
  dimension_names <- tolower(
    vapply(ce_result$GroupDefinitions, `[[`, "Key", FUN.VALUE = character(1))
  )

  if (length(dimension_names) > 0) {
    res_by_time <- Filter(\(y) length(y$Groups) > 0, ce_result$ResultsByTime)
  } else {
    res_by_time <- ce_result$ResultsByTime
  }

  df_list <- lapply(
    res_by_time,
    results_by_time_to_df,
    keynames = dimension_names,
    metric = metric
  )

  purrr::list_rbind(df_list)
}

#' Convert AWS Cost Explorer results to a data frame
#'
#' @param x A list of results from AWS Cost Explorer.
#' @param keynames A character vector of dimension names. Optional.
#' @param metric A single string specifying the metric to extract.
#'
#' @returns
#' A data frame containing columns for the start and end dates, any group dimensions
#' specified in `keynames`, the requested metric value, and whether the value is
#' estimated.
#'
#' @noRd
results_by_time_to_df <- function(x, keynames, metric) {
  start_date <- lubridate::as_datetime(x$TimePeriod$Start)
  end_date <- lubridate::as_datetime(x$TimePeriod$End)

  if (length(keynames) > 0) {
    # Grouped Query
    keys <- purrr::list_rbind(
      lapply(x$Groups, \(x) {
        as.data.frame(
          t(x[["Keys"]])
        )
      })
    )

    names(keys) <- keynames

    metric_val <- vapply(
      x$Groups,
      \(x) {
        as.numeric(x$Metrics[[metric]]$Amount)
      },
      FUN.VALUE = numeric(1)
    )
  } else {
    # No groups
    metric_val <- as.numeric(x$Total[[metric]]$Amount)
    keys <- NULL
  }

  estimated <- x$Estimated

  dplyr::bind_cols(
    start_date = start_date,
    end_date = end_date,
    keys,
    {{ metric }} := metric_val,
    estimated = estimated
  )
}

# Aggregate services to maximum ten categories to simplify
# visualization and align with AWS CE colour palette
#' Aggregate cost explorer service categories for visualization
#' and align with AWS CE colour palette
#'
#' @param df A data frame.
#' @param n_categories Optional. A single integer indicating the maximum number of service categories.
#' @param cost_col Column name containing cost values.
#'
#' @returns
#' A grouped data frame summarizing costs by date range and service category.
#' Services with lower total costs are grouped into an "Other" category if the
#' number of unique services exceeds `n_categories`.
#'
#' @export
ce_categories <- function(df, n_categories = 10, cost_col) {
  if (length(unique(df$service)) > n_categories) {
    top_services <- df |>
      dplyr::group_by(.data$service) |>
      dplyr::summarise(total_cost = sum({{ cost_col }})) |>
      dplyr::slice_max(
        .data$total_cost,
        n = n_categories - 1,
        with_ties = FALSE
      ) |>
      dplyr::pull(.data$service)

    df$service[!df$service %in% top_services] <- "Other"
  }

  df |>
    dplyr::group_by(.data$start_date, .data$end_date, .data$service) |>
    dplyr::summarise(
      "{{ cost_col }}" := sum({{ cost_col }}, na.rm = TRUE),
      .groups = "drop"
    )
}

#' AWS Cost Explorer palette
#'
#' @param n number of categories
#'
#' @returns
#' A character vector of `n` distinct hex color codes.
#'
#' @export
aws_ce_palette <- function(n) {
  pal <- c(
    "#6889e9",
    "#c33d69",
    "#2ea597",
    "#8356cd",
    "#e07a41",
    "#0166ab",
    "#952248",
    "#0b7164",
    "#6135a6",
    "#9a7b09"
  )

  rev(pal[seq(1, n)])
}
