#' Split a string on commas
#'
#' @param string A character vector with, at most, one element.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' x <- "a,b,c"
#' str_split_comma(x)
#'
#' y <- "a.b.c"
#' str_split_comma(y)
str_split_comma <- function(string) {
  stopifnot(is.character(string), length(string) <= 1)
  if (length(string) == 1) {
    stringr::str_split(string = string, pattern = ",")[[1]]
  } else {
    character()
  }
}
