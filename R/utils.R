#' Unsanitize directory names
#'
#' Where directory names have had special characters replaced through url-encoding,
#' revert them back to their unescaped forms
#'
#' @param x A character vector of sanitized directory names.
#'
#' @returns
#' A character vector with special characters restored from their sanitized form.
#'
#' @export
unsanitize_dir_names <- function(x) {
  x <- gsub("-2d", "-", x)
  x <- gsub("-2e", ".", x)
  x <- gsub("-40", "@", x)
  x <- gsub("-5f", "_", x)
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
