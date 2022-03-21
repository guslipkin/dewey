#' Compute a lagged version of the input data with multiple lags at once
#'
#' @export
#'
#' @param x A vector, single column matrix, or univariate time series. Can also
#'   be a multi-column matrix if `k` is length 1.
#' @param k (Optional) An integer vector containing a number of lags. Defaults
#'   to 1
#'
#' @return Returns a data.frame of the lagged variable. The number of rows is
#'   the same as the length of the input vector. The number of columns is the
#'   number of lags to be used. Each column retains the name of the original
#'   variable and includes the number of lags used for that column. If `x` is a
#'   multi-column matrix, returns a matrix of the same number of columns with no
#'   names.
#'
#' @examples
#' # Creating dummy data
#' x <- rnorm(10)
#' # for lags 1-5
#' lagMultiple(x, 1:5)
#'
#' # 1 lag with a matrix of dummy data
#' lagMultiple(matrix(1:100, 10, 10), 1)
lagMultiple <- function(x, k = 1) {

  # if x is a matrix, lag  must be length 1
  if(is.matrix(x) & length(k) > 1) {
    stop("if `x` is a matrix, `k` must be length 1")
  }

  # stop if not all values of k are integers
  if (!all(isInteger(k))) {
    stop("k must be an integer or integer vector")
  }

  if (is.matrix(x)) {
    # create a matrix of lagged values
    df <- apply(x, 2, function(y) {
      # shift the values in the appropriate direction based on the number of lags
      if (k < 0) { c(rep(NA, abs(k)), y[1:(length(y) - abs(k))]) }
      else { c(y[(k + 1):length(y)], rep(NA, k)) }
    })
  } else {
    df <- data.frame(sapply(k, function(k) {
      # shift the values in the appropriate direction based on the number of lags
      if (k < 0) { c(rep(NA, abs(k)), x[1:(length(x) - abs(k))]) }
      else { c(x[(k + 1):length(x)], rep(NA, k)) }
    }))

    # get the variable name and rename the columns with the correct lag number
    colnames(df) <- paste0(deparse(substitute(x)), "_lag", k)
  }

  # return the data.frame
  return(df)
}
