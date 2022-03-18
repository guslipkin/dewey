#' A helper function
#'
#' @noRd
#'
#' @param x A vector
#'
#' @return Returns true if x is not an integer or integer vector
#'
#' @examples
#' # returns FALSE
#' isNotInteger(5)
#'
#' # returns TRUE
#' isNotInteger(5.1)
isNotInteger <- function(x) {
  # return true if x is not an integer or integer vector
  return(!is.numeric(x) | (x != as.integer(x)))
}
