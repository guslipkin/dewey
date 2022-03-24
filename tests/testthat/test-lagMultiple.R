test_that("x can be a matrix when lag and differences aren't vectors", {
  expect_equal(lagMultiple(matrix(1:10, 10, 10), -1),
               matrix(c(NA, 1:9), 10, 10))
  expect_error(lagMultiple(matrix(1:100, 10, 10), 1:5),
               "if `x` is a matrix, `k` must be length 1")
})

test_that("lags are integers or integer vectors", {
  expect_error(lagMultiple(1:10, 1.1),
               "k must be an integer or integer vector")
})

test_that("output name is overridden with the 'name' argument", {
  expect_equal(names(lagMultiple(1:10, 1, "var")), "var_l1")
})

test_that("returns a data.frame", {
  expect_s3_class(lagMultiple(1:10, 1, "var"), "data.frame")
})
