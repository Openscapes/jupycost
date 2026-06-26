#' Unsanitize directory names
#'
#' Where directory names have had special characters replaced through url-encoding,
#' revert them back to their unescaped forms.
#'
#' @param x A character vector of sanitized directory names.
#'
#' @returns
#' A character vector with special characters restored from their sanitized form.
#'
#' @export
unsanitize_dir_names <- function(x) {
  lookup <- c("-2d" = "-", "-2e" = ".", "-40" = "@", "-5f" = "_")
  pattern <- paste(names(lookup), collapse = "|")
  m <- gregexpr(pattern, x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), \(matches) {
    unname(lookup[matches])
  })
  x
}

check_valid_date <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (inherits(x, "Date") && length(x) == 1L) {
    return(x)
  }

  date <- try(as.Date(x), silent = TRUE)

  if (
    inherits(date, "try-error") ||
      length(date) > 1 ||
      all(is.na(date)) ||
      is.null(date)
  ) {
    cli::cli_abort(
      "{.arg {arg}} must be a length 1 Date or POSIXt object, or character in a standard unambiguous date format",
      arg = arg,
      call = call
    )
  }

  date
}

#' Set environment variables for testing
#'
#' This is kind of a remake of withr::local_envvar() but gets the env var values
#' from the prefixed versions of the vars.
#'
#' @param org One of `"nasa"` or `"nmfs"`.
#' @param env The environment in which to restore the original environment variables.
#'
#' @returns
#' `TRUE` for each environment variables successfully set and are not-empty.
#' Otherwise, `FALSE`. Environment variables are automatically restored to
#' their original values when the environment is cleaned up.
#' @noRd
set_env_vars <- function(org = c("nasa", "nmfs"), env = parent.frame()) {
  org <- match.arg(org)

  env_var_names <- c(
    "GRAFANA_TOKEN",
    "AWS_ACCESS_KEY_ID",
    "AWS_SECRET_ACCESS_KEY",
    "AWS_REGION"
  )

  env_vars_init <- Sys.getenv(env_var_names)

  env_vars <- vapply(
    env_var_names,
    \(x) {
      Sys.getenv(
        glue::glue("{toupper(org)}_{x}")
      )
    },
    FUN.VALUE = character(1),
    USE.NAMES = TRUE
  )

  env_vars["AWS_REGION"] <- "us-east-1"

  were_vars_set <- do.call(Sys.setenv, as.list(env_vars))

  withr::defer(do.call(Sys.setenv, as.list(env_vars_init)), envir = env)

  if (any(!were_vars_set)) {
    cli::cli_warn(
      "{.var {names(env_vars)[!were_vars_set]}} env var{?s} not set"
    )
  }

  empty_vars <- !nzchar(Sys.getenv(names(env_vars)))

  stats::setNames(were_vars_set & !empty_vars, names(env_vars))
}


#' Convert Prometheus numeric date to POSIXct
#'
#' @param x A numeric vector of unix timestamps.
#'
#' @returns
#' A POSIXct vector in UTC timezone.
#'
#' @noRd
prom_date <- function(x) {
  as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC")
}

#' Format time as string in the format that prometheus expects
#'
#' @param x A Date, POSIXt time object or character string in UTC
#'
#' @returns
#' A string in ISO 8601 format with UTC timezone (e.g. "2024-01-01T12:00:00Z").
#'
#' @noRd
time_string <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!inherits(x, c("POSIXt", "character", "Date")) || length(x) != 1) {
    cli::cli_abort(
      "{.arg {arg}} must be a length 1 Date or POSIXt object, or character in a standard unambiguous date format",
      arg = arg,
      call = call
    )
  }
  format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
}

#' Parse Prometheus step format to seconds
#'
#' Parse a Prometheus step string to seconds
#'
#' @param step A single numeric (returned as-is) or a Prometheus duration
#'   string composed of one or more `<integer><unit>` pairs. Valid units are
#'   `ms` (milliseconds), `s` (seconds), `m` (minutes), `h` (hours),
#'   `d` (days), `w` (weeks), `y` (years). Examples: `"1h"`, `"1h30m"`,
#'   `"24h0m0s"`, `"500ms"`, `"1d"`.
#' @returns A single numeric value representing the total number of seconds.
#' @noRd
parse_step_to_seconds <- function(step) {
  if (is.numeric(step)) {
    return(step)
  }

  if (!is.character(step) || length(step) != 1L) {
    cli::cli_abort(
      c(
        "{.arg step} must be a single number or a Prometheus duration string.",
        "i" = "Examples: {.code 3600}, {.code \"1h\"}, {.code \"1h30m\"}, {.code \"24h0m0s\"}"
      )
    )
  }

  # Valid Prometheus duration: one or more <digits><unit> pairs.
  # ms must appear before m in the alternation so "500ms" is not parsed as
  # "500 minutes" followed by a stray "s".
  valid_re <- "^([0-9]+(ms|s|m|h|d|w|y))+$"
  if (!grepl(valid_re, step, perl = TRUE)) {
    cli::cli_abort(
      c(
        "{.arg step} {.val {step}} is not a valid Prometheus duration string.",
        "i" = "Valid units: {.code ms}, {.code s}, {.code m}, {.code h}, {.code d}, {.code w}, {.code y}",
        "i" = "Examples: {.code \"1h\"}, {.code \"30m\"}, {.code \"1h30m0s\"}, {.code \"500ms\"}"
      )
    )
  }

  # Strip ms tokens before matching bare m and s
  step_no_ms <- gsub("[0-9]+ms", "", step)

  extract_unit <- function(pattern, str) {
    matches <- regmatches(str, gregexpr(pattern, str, perl = TRUE))[[1]]
    if (length(matches) == 0) {
      return(0)
    }
    sum(as.numeric(gsub("[a-z]+", "", matches)))
  }

  ms_val <- extract_unit("[0-9]+ms", step)
  y_val <- extract_unit("[0-9]+y", step_no_ms)
  w_val <- extract_unit("[0-9]+w", step_no_ms)
  d_val <- extract_unit("[0-9]+d", step_no_ms)
  h_val <- extract_unit("[0-9]+h", step_no_ms)
  m_val <- extract_unit("[0-9]+m", step_no_ms)
  s_val <- extract_unit("[0-9]+s", step_no_ms)

  y_val *
    365 *
    86400 +
    w_val * 7 * 86400 +
    d_val * 86400 +
    h_val * 3600 +
    m_val * 60 +
    s_val +
    ms_val / 1000
}

glue_promql <- function(..., .envir = parent.frame()) {
  glue::glue(..., .envir = .envir, .open = "<", .close = ">")
}
