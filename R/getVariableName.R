#' A helper function
#'
#' @noRd
#'
#' @param x An object
#' @param name A string
#'
#' @return Attempts to return the variable name of `x`. If that is not possible,
#'   returns `name`. If name is not supplied "X" is returned as a default value.
#'
#' @examples
#' # returns "var"
#' getVariableName("test$var")
#'
#' returns "X"
#' getVariableName(seq(1, 10, 1))
getVariableName <- function(x, name = NULL) {
  if (is.null(name)) {
    name <- gsub("[a-zA-z][a-zA-Z0-9_\\.]*\\$", "", deparse(substitute(x)))
    if (grepl("(^[0-9]|[^a-zA-Z0-9_\\.])", name)) {
      warning("Unable to get the variable name from '",
              name,
              "', defaulting to 'X'")
      name <- "X"
    }
  }
  return(name)
}
