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
