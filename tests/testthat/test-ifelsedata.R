test_that("values are selected from x when true", {
  expect_equal(ifelsedata(data.frame(matrix(1:100, 10, 10)),
                          data.frame(matrix(rep(50, 100), 10, 10)),
                          "x > y"),
               data.frame(matrix(c(rep(NA, 50), 51:100), 10, 10)))
  expect_equal(ifelsedata(data.frame(matrix(1:100, 10, 10)),
                          data.frame(matrix(c(rep(FALSE, 50),
                                              rep(TRUE, 50)), 10, 10))),
               data.frame(matrix(c(rep(NA, 50), 51:100), 10, 10)))
})

test_that("arg can be ignored if y is logical", {
  expect_equal(ifelsedata(data.frame(matrix(1:100, 10, 10)),
                          data.frame(matrix(c(rep(FALSE, 50),
                                              rep(TRUE, 50)), 10, 10))),
               data.frame(matrix(c(rep(NA, 50), 51:100), 10, 10)))
  expect_error(ifelsedata(data.frame(matrix(1:100, 10, 10)),
                          data.frame(matrix(rep(50, 100), 10, 10))),
               "No argument supplied and 'y' is not all logical")
})

test_that("x only accepts data.frame and matrix objects", {
  expect_equal(ifelsedata(data.frame(matrix(1:100, 10, 10)), TRUE),
               data.frame(matrix(1:100, 10, 10)))
  expect_equal(ifelsedata(matrix(1:100, 10, 10), TRUE),
               data.frame(matrix(1:100, 10, 10)))
  expect_error(ifelsedata(1:100, TRUE), "Unable to coerce 'x' to data.frame")
})

test_that("y accepts data.frames, matrices, and vectors where length(y)==nrow(x) or is length 1", {
  expect_equal(ifelsedata(data.frame(matrix(1:100, 10, 10)),
                          data.frame(matrix(TRUE, 10, 10))),
               data.frame(matrix(1:100, 10, 10)))
  expect_equal(ifelsedata(matrix(1:100, 10, 10), rep(TRUE, 10)),
               data.frame(matrix(1:100, 10, 10)))
  expect_equal(ifelsedata(matrix(1:100, 10, 10), TRUE),
               data.frame(matrix(1:100, 10, 10)))
  expect_error(ifelsedata(data.frame(matrix(1:100, 10, 10)), 1:2),
               "Unable to coerce 'y' to data.frame")
})

test_that("column names are appropriately matched according to their location in x", {
  x <- data.frame(matrix(1:100, 10, 10))
  colnames(x) <- nameVec <- paste0("X", sample(1:10, 10))
  expect_equal(colnames(ifelsedata(x, data.frame(matrix(TRUE, 10, 10)),
                          matchCols = TRUE)),
               nameVec)

  expect_equal(colnames(ifelsedata(data.frame(matrix(1:100, 10, 10)), TRUE,
                          matchCols = TRUE)),
               colnames(data.frame(matrix(1:100, 10, 10))))
})

test_that("data.frames of different sizes return the smaller size", {
  a <- data.frame(matrix(1:25, 5, 5))
  b <- data.frame(matrix(1:100, 10, 10))

  expect_equal(ifelsedata(a, b, "x == y"),
               data.frame(matrix(c(1:5, rep(NA, 20)), 5, 5)))
  expect_equal(ifelsedata(b, a, "x == y"),
               data.frame(matrix(c(1:5, rep(NA, 20)), 5, 5)))
})

test_that("returns a data.frame", {
  expect_s3_class(ifelsedata(matrix(1:100, 10, 10), TRUE), "data.frame")
})
