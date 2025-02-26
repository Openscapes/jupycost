skip_if_env_vars_not_set <- function() {
  env_var_names <- c(
    "GRAFANA_TOKEN",
    "AWS_ACCESS_KEY_ID",
    "AWS_SECRET_ACCESS_KEY",
    "AWS_REGION"
  )

  empty_vars <- !nzchar(Sys.getenv(env_var_names))
  if (any(empty_vars)) {
    skip_cli("{.var {env_var_names[empty_vars]}} env var{?s} not set")
  }
}

skip_cli <- function(x, ..., env = parent.frame()) {
  testthat::skip(cli::cli_fmt(cli::cli_text(x, ..., .envir = env)))
}
