#' Split a string and convert to numeric
#'
#' @param string A character vector with, at most, one element.
#' @inheritParams stringr::str_split
#'
#' @return A numeric vector. Non-numeric elements are coerced to NA with a warning.
#' @export
#'
#' @examples
#' x <- "1,2,3"
#' str_split_nums(x, pattern = ",")
#'
#' y <- "a,2,3"
#' str_split_nums(y, pattern = ",")
str_split_nums <- function(string, pattern) {
  stopifnot(is.character(string), length(string) <= 1)
  if (length(string) == 1) {
    parts <- stringr::str_split(string = string, pattern = pattern)[[1]]
    result <- suppressWarnings(as.numeric(parts))
    if (any(is.na(result))) {
      warning("NAs introduced by coercion")
    }
    result
  } else {
    numeric()
  }
}
