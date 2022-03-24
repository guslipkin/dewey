test_that("the variable name is returned", {
  var <- data.frame(matrix(1:100, 10, 10))
  expect_equal(getVariableName(var$X1), "X1")

  var <- 1:10
  expect_equal(getVariableName(var), "var")
})

test_that("the defined name is returned", {
  expect_equal(getVariableName(1:10, "var"), "var")
})

test_that("'X' is returned", {
  expect_warning(getVariableName(1),
                 "Unable to get the variable name from '1', defaulting to 'X'")
})

test_that("a vector length 1 is returned", {
  expect_vector(getVariableName(1, "var"))
  expect_equal(length(getVariableName(1, "var")), 1)
})
