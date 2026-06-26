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
#' @param step Numeric (seconds) or character string like "1h0m0s"
#' @returns Numeric seconds
#' @noRd
parse_step_to_seconds <- function(step) {
  if (is.numeric(step)) {
    return(step)
  }

  hours <- if (grepl("h", step)) {
    as.numeric(sub("([0-9]+)h.*", "\\1", step))
  } else {
    0
  }
  minutes <- if (grepl("m", step)) {
    as.numeric(sub(".*?([0-9]+)m.*", "\\1", step))
  } else {
    0
  }
  seconds <- if (grepl("s", step)) {
    as.numeric(sub(".*?([0-9]+)s.*", "\\1", step))
  } else {
    0
  }

  hours * 3600 + minutes * 60 + seconds
}

glue_promql <- function(..., .envir = parent.frame()) {
  glue::glue(..., .envir = .envir, .open = "<", .close = ">")
}
