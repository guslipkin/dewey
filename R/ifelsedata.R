ifelsedata <- function(x, y, arg) {
  r <- min(c(nrow(d1), nrow(d2)))
  c <- min(c(ncol(d1), ncol(d2)))

  d1 <- d1[1:r, 1:c]
  d2 <- d2[1:r, 1:c]

  l1 <- sapply(as.list(d1), as.vector)
  l2 <- sapply(as.list(d2), as.vector)

  expr <- paste("ifelse(l1", arg, "l2, l1, NA)", collapse = "")
  d <- data.frame(eval(parse(text=expr)))
}
