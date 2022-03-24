#' Fast data.frame comparisons at the cell level
#'
#' @export
#'
#' @param x A `data.frame` or `matrix`
#' @param y A `data.frame` or `matrix` of the same dimensions as `x` or a vector
#'   with a length matching the rows of `x` or a length 1. If `y` is a
#'   `data.frame` or `matrix` with dimensions different than `x`, the larger
#'   will be trimmed to match the dimensions of the smaller.
#' @param arg (Optional) A logical test expression including `x` and `y`. If
#'   `arg` is not included, it is assumed that all values of `y` are logical.
#' @param matchCols (Optional) A boolean that determines if columns will be
#'   matched based on name or position. Columns will be returned in the order
#'   they are in in `x`. Columns not present in `x` will not be returned.
#'   Defaults to `FALSE`.
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

  # attempt to coerce x and y if they are not data.frames
  if (!is.data.frame(x)) {
    # if x is a matrix, coerce it. if not, don't try.
    if (is.matrix(x)) { x <- data.frame(x) }
    else { stop("Unable to coerce 'x' to data.frame") }
  }

  # if y is not a data.frame, warn about coercion
  if (!is.data.frame(y)) {
    # if y is a matrix, coerce it. if not, try if it's a vector. if not, don't try
    if (is.matrix(y)) { y <- data.frame(y) }
    else if (is.vector(y) & length(y) %in% c(1, nrow(x))) {
      # y can be a vector of the same length as the number of rows of x
      # y can also be a vector of length 1
      y <- data.frame(matrix(y, nrow(x), ncol(x)))
    } else { stop("Unable to coerce 'y' to data.frame") }
  }

  # get column names so that `matchCols` can be tested
  colsX <- colnames(x)
  colsY <- colnames(y)
  cols <- colsX[colsX %in% colsY]
  cols <- cols[order(match(colsX, cols))]
  cols <- cols[!is.na(cols)]
  # if `matchCols` is true, filter based on column names
  if(matchCols & length(cols) == 0) {
    stop("There are no matching columns in `x` and `y`")
  } else if (matchCols) {
    x <- x[, cols]
    y <- y[, cols]
  }

  # match dimensions to the smaller if dimensions are not equal
  if (!all(dim(x) == dim(y))) {
    # get the smaller dimensions
    r <- min(c(nrow(x), nrow(y)))
    c <- min(c(ncol(x), ncol(y)))

    # trim data to size
    x <- x[1:r, 1:c]
    y <- y[1:r, 1:c]
  }

  # create the expression
  # if `arg` is missing, evaluate `x` based on contents of `y` assuming `y` is a
  # boolean
  if(is.null(arg)) {
    if(all(apply(y, c(1, 2), is.logical))) { expr <- "ifelse(y, x, NA)" }
    else { stop("No argument supplied and 'y' is not all logical") }
  }
  else { expr <- paste("ifelse(", arg, ", x, NA)", collapse = "") }

  # convert the data to lists of vectors
  x <- sapply(as.list(x), as.vector)
  y <- sapply(as.list(y), as.vector)

  # evaluate the expression and return the result
  return(data.frame(eval(parse(text=expr))))
}
