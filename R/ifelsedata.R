#' Fast data.frame comparisons at the cell level
#'
#' @export
#'
#' @param x A `data.frame` of the same dimensions as `y`.
#' @param y A `data.frame` of the same dimensions as `x`.
#' @param arg (Optional) A logical test such as `==` or `>` in string format. If
#'   `arg` is not included, it is assumed that all values of `y` are logical.
#' @return Returns a data.frame of the smallest size by rows and columns. The
#'   cells returned are from `x` if the test passes and `NA` if it does not
#'   pass.
#' @examples
#' # create dummy data
#' x <- data.frame(matrix(data = sample(1:10, 100, TRUE), nrow = 10, ncol = 10))
#' y <- data.frame(matrix(data = sample(c(TRUE, FALSE), 100, TRUE),
#'                        nrow = 10,
#'                        ncol = 10))
#'
#' # test for equality
#' ifelsedata(x, y, "==")
#' ifelsedata(x, y)
ifelsedata <- function(x, y, arg = NULL) {
  # create the expression
  #
  # if `arg` is missing, evaluate `x` based on contents of `y` assuming `y` is a
  # boolean
  if(missing(arg))
    expr <- paste("ifelse(x, y, NA)", collapse = "")
  else
    expr <- paste("ifelse(x", arg, "y, x, NA)", collapse = "")

  # get the smaller dimensions
  r <- min(c(nrow(x), nrow(y)))
  c <- min(c(ncol(x), ncol(y)))

  # trim data to size
  x <- x[1:r, 1:c]
  x <- x[1:r, 1:c]

  # convert the data to lists of vectors
  x <- sapply(as.list(x), as.vector)
  y <- sapply(as.list(y), as.vector)

  # evaluate the expression and return the result
  return(data.frame(eval(parse(text=expr))))
}
