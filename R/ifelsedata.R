ifelsedata <- function(x, y, arg = NULL) {
  # create the expression
  # if `arg` is missing, evaluate `x` based on contents of `y`
  #   assuming `y` is a boolean
  if(missing(arg))
    expr <- paste("ifelse(l2, l1, NA)", collapse = "")
  else
    expr <- paste("ifelse(l1", arg, "l2, l1, NA)", collapse = "")

  # get the smaller dimensions
  r <- min(c(nrow(d1), nrow(d2)))
  c <- min(c(ncol(d1), ncol(d2)))

  # trim data to size
  d1 <- d1[1:r, 1:c]
  d2 <- d2[1:r, 1:c]

  # convert the data to lists of vectors
  l1 <- sapply(as.list(d1), as.vector)
  l2 <- sapply(as.list(d2), as.vector)

  # evaluate the expression and return the result
  data.frame(eval(parse(text=expr)))
}
