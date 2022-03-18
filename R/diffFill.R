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

  # stop if not all values of lag or differences are integers
  if (any(sapply(lag, isNotInteger)) | any(lag < 1)) {
    stop("lag must be an integer or integer vector and >= 1")
  }
  if (any(sapply(differences, isNotInteger)) | any(differences < 1)) {
    stop("differences must be an integer or integer vector and >= 1")
  }

  # make sure the lag and differences are the same length
  if(length(lag) != length(differences)) {
    # warn if lag or differences, but not both are length 1, but warn if the
    # other is not
    # stop if the warning criteria are not met
    if(length(lag) == 1) {
      warning("'lag' is length 1, mapping 'lag' to all values of 'differences'")
      lag <- rep(lag, length(differences))
    } else if(length(differences) == 1) {
      warning("'differences' is length 1, mapping 'differences' to all values of 'lag'")
      differences <- rep(differences, length(lag))
    } else {
      stop("'lag' and 'differences' must be the same length or length 1")
    }
  }

  # append the appropriate number of NA values to the diff and build into a
  # data.frame
  df <- data.frame(sapply(1:length(lag), function(y) {
    c(rep(NA, lag[y] * differences[y]), diff(x, lag[y], differences[y], ...))
  }))

  # get the variable name and rename the columns with the correct lag and
  # difference number
  colnames(df) <-
    paste0(deparse(substitute(x)), "_l", lag, "d", differences
    )[1:min(length(lag), length(differences))]

  # return the data.frame
  return(df)
}
