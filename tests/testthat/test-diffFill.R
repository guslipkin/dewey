test_that("only integer values >= 1 for lag are accepted", {
  expect_error(diffFill(1:10, -1, 1),
               "lag must be an integer or integer vector and >= 1")
  expect_error(diffFill(1:10, 1.1, 1),
               "lag must be an integer or integer vector and >= 1")
})

test_that("only integer values >= 1 for differences are accepted", {
  expect_error(diffFill(1:10, 1, -1),
               "differences must be an integer or integer vector and >= 1")
  expect_error(diffFill(1:10, 1, 1.1),
               "differences must be an integer or integer vector and >= 1")
})

test_that("x can be a matrix when lag and differences aren't vectors", {
  expect_equal(diffFill(matrix(1:100, 10, 10), 1, 1),
               matrix(c(NA, rep(1, 9)), 10, 10))
  expect_error(diffFill(matrix(1:100, 10, 10), 1:5, 1:5),
               "if `x` is a matrix, `lag` and `differences` must be length 1")
})

test_that("output name is overridden with the 'name' argument", {
  expect_equal(names(diffFill(1:10, 1, 1, "var")), "var_l1d1")
})

test_that("returns a data.frame", {
  expect_s3_class(diffFill(1:10, 1, 1, "var"), "data.frame")
})
