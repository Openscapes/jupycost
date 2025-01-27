unsanitize_dir_names <- function(x) {
  x <- gsub("-2d", "-", x)
  x <- gsub("-2e", ".", x)
  x <- gsub("-40", "@", x)
  x <- gsub("-5f", "_", x)
  x
}
