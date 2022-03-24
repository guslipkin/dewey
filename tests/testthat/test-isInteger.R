test_that("integers are identified", {
  expect_true(isInteger(5))
  expect_true(isInteger(5.0))
  expect_true(all(isInteger(c(5, 5.0))))

  expect_false(isInteger(5.1))
  expect_false(all(isInteger(c(5, 5.1))))
})

test_that("character numbers are handled appropriately", {
  expect_warning(isInteger("5.0"),
                 "Character input accepted, attempting to coerce to numeric")
})

test_that("unacceptable input is handled appropriately", {
  expect_error(
    expect_warning(
      expect_warning(
        isInteger("e"),
        "Character input accepted, attempting to coerce to numeric"
      ),
      "NAs introduced by coercion"
    ),
    "Unable to coerce input to numeric"
  )
  expect_error(isInteger(TRUE), "`isInteger` only accepts numeric input")
})
