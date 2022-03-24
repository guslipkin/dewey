#' A helper function
#'
#' @noRd
#'
#' @param x A vector
#'
#' @return A vectorised form of `is.integer` that ignores decimal places but not
#'   values
#'
#' @examples
#' # returns TRUE
#' isInteger(5)
#' isInteger(5.0)
#'
#' # returns FALSE
#' isInteger(5.1)
isInteger <- function(x) {

  # apply the checker function to every input
  sapply(x, function(x) {
    # attempt to coerce if input is a character
    if (!is.numeric(x) & !is.character(x)) {
      stop("`isInteger` only accepts numeric input")
    } else if (is.character(x)) {
      warning("Character input accepted, attempting to coerce to numeric")
      x <- as.numeric(x)
      # if there are any errors, stop the function
      if (any(is.na(x)))
        stop("Unable to coerce input to numeric")
    }
    # return true if x is an integer or integer vector
    return(round(x, 0) == x)
  })

}
