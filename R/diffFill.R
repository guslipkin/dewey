#' An extension on `diff` from base R
#'
#' @export
#'
#' @param x a numeric vector or matrix containing the values to be differenced
#' @param lag (Optional) an integer indicating which lag to use. Defaults to 1
#' @param differences (Optional) an integer indicating the order of the
#'   difference. Defaults to 1
#' @param ... (Optional) further arguments to be passed to or from methods
#'
#' @return  Returns a vector of the same length as the input, otherwise behaves
#'   the same as \code{\link[base]{diff}}
#'
#' @examples
#' # Creating dummy data
#' x <- rnorm(100)
#' # for 1 lag and 2 differences
#' diffFill(x, 1, 2)
diffFill <- function(x, lag = 1, differences = 1, ...) {
  # append the appropriate number of NA values to the diff
  return(c(rep(NA, lag * differences), diff(x, lag, differences, ...)))
}
