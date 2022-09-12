#' A riff on `dplyr::summarise` that returns columns with values for all rows
#'
#' @export
#'
#' @param .data,...,.groups Takes all the same arguments as
#'   \code{\link[dplyr]{summarise}}
#'
#' @return A data.frame identical to `.data` with new columns representing the
#'   summarisations made after grouping. Values in the new columns are repeated
#'   as necessary to maintain the number of rows of the original data.frame.
#'
#' @importFrom dplyr summarise select group_vars n
#'
#' @examples
#' # Get mean mpg by number of cyl from mtcars
#' mtcars |>
#'   dplyr::group_by(cyl) |>
#'   summarise_all_rows(mean_mpg = mean(mpg))
summarise_all_rows <- function(.data, ..., .groups = NULL) {
  # easier to reference
  df <- .data

  # get the new columns from dplyr::summarise
  # add a column with the number of rows per group to expand with
  new_cols <- df |>
    summarise(..., n = n(), .groups) |>
    select(- group_vars(df))

  # split the number of items and values for ease of use
  n <- new_cols$n
  new_cols <- as.list(new_cols[, -ncol(new_cols)])

  # return a data.frame that has been properly expanded
  new_cols <- lapply(1:length(new_cols), function(x) {
    rep(new_cols[[x]], n)
  }) |>
    `names<-`(names(new_cols)) |>
    data.frame()

  # give the user all the original data and the new columns
  cbind(df, new_cols)
}
