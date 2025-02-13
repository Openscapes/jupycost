#' Set environment variables for testing
#'
#' This is kind of a remake of withr::local_envvar() but gets the env var values
#' from the prefixed versions of the vars, and does the test skips if not set.
#'
#' @param org One of `"nasa"` or `"nmfs"`.
#' @param env The environment in which to restore the original environment variables.
#'
#' @returns
#' `TRUE` if all environment variables were successfully set and are non-empty.
#' Otherwise, skips the test and returns `FALSE`. Environment variables are
#' automatically restored to their original values when the environment is cleaned up.
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
    skip_cli("{.var {names(env_vars)[!were_vars_set]}} env var{?s} not set")
    return(FALSE)
  }

  empty_vars <- !nzchar(Sys.getenv(names(env_vars)))

  if (any(empty_vars)) {
    skip_cli("{.var {names(env_vars)[empty_vars]}} env var{?s} empty")
    return(FALSE)
  }

  TRUE
}

skip_cli <- function(x, ...) {
  testthat::skip(cli::cli_fmt(cli::cli_text(x, ...)))
}
