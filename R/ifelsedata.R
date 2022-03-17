#' Fast data.frame comparisons at the cell level
#'
#' @export
#'
#' @param x A `data.frame` of the same dimensions as `y`.
#' @param y A `data.frame` of the same dimensions as `x` or a vector of length
#'   one.
#' @param arg (Optional) A logical test expression including `x` and `y`. If
#'   `arg` is not included, it is assumed that all values of `y` are logical.
#' @param matchCols (Optional) A boolean that determines if columns will be
#'   matched based on name or position. Default is `FALSE`
#' @return Returns a data.frame of the smallest size by rows and columns. The
#'   cells returned are from `x` if the test passes and `NA` if it does not
#'   pass.
#' @examples
#' # create dummy data
#' x <- data.frame(matrix(data = sample(1:10, 100, TRUE), nrow = 10, ncol = 10))
#' y <- data.frame(matrix(data = sample(1:10, 100, TRUE), nrow = 10, ncol = 10))
#'
#' # test for equality
#' ifelsedata(x, y, "x >= y | x == y - 2")
#'
#' # rename x columns
#' colnames(x) <- paste0("X", 5:14)
#' # match with column names
#' ifelsedata(x, y, "x >= y | x == y - 2", TRUE)
#'
#' # match based on booleans in y
#' y <- data.frame(matrix(data = sample(c(TRUE, FALSE), 100, TRUE),
#'                        nrow = 10,
#'                        ncol = 10))
#' # test based on TRUE/FALSE in y
#' ifelsedata(x, y)
ifelsedata <- function(x, y, arg = NULL, matchCols = FALSE) {

  if(is.numeric(y) & length(y) == 1)
    y <- data.frame(matrix(data = y, nrow = nrow(x), ncol = ncol(x)))
  # get column names so that `matchCols` can be tested
  colsX <- colnames(x)
  colsY <- colnames(y)
  cols <- colsX[colsX %in% colsY]
  # if `matchCols` is true, filter based on column names
  if(matchCols & length(cols) == 0)
    stop("There are no matching columns in `x` and `y`")
  if (matchCols) {
    x <- x[, cols]
    y <- y[, cols]
  } else {
    # get the smaller dimensions
    r <- min(c(nrow(x), nrow(y)))
    c <- min(c(ncol(x), ncol(y)))

    # trim data to size
    x <- x[1:r, 1:c]
    x <- x[1:r, 1:c]
  }

  # create the expression
  # if `arg` is missing, evaluate `x` based on contents of `y` assuming `y` is a
  # boolean
  if(missing(arg))
    expr <- paste("ifelse(x, y, NA)", collapse = "")
  else
    expr <- paste("ifelse(", arg, ", x, NA)", collapse = "")
  # expr <- paste("ifelse(x", arg, "y, x, NA)", collapse = "")

  # convert the data to lists of vectors
  x <- sapply(as.list(x), as.vector)
  y <- sapply(as.list(y), as.vector)

  # evaluate the expression and return the result
  return(data.frame(eval(parse(text=expr))))
}
