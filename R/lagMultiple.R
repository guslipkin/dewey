#' An extension on `diff` from base R
#'
#' @export
#'
#' @param x A vector, single column matrix, or univariate time series
#' @param k (Optional) An integer vector containing a number of lags. Defaults
#'   to 1
#'
#' @return Returns a data.frame of the lagged variable. The number of rows is
#'   the same as the length of the input vector. The number of columns is the
#'   number of lags to be used. Each column retains the name of the original
#'   variable and includes the number of lags used for that column.
#'
#' @examples
#' # Creating dummy data
#' x <- rnorm(100)
#' # for lags 1-5
#' lagMultiple(x, 1:5)
lagMultiple <- function(x, k = 1) {

  # stop if not all values of k are integers
  if (any(sapply(k, isNotInteger))) {
    stop("k must be an integer or integer vector")
  }

  # create a data.frame of lagged values
  df <- data.frame(sapply(k, function(k) {
    # shift the values in the appropriate direction based on the number of lags
    if (k < 0) { c(rep(NA, abs(k)), x[1:(length(x) - abs(k))]) }
    else { c(x[(k + 1):length(x)], rep(NA, k)) }
  }))

  # get the variable name and rename the columns with the correct lag number
  colnames(df) <- paste0(deparse(substitute(x)), "_lag", k)
  # return the data.frame
  return(df)
}
