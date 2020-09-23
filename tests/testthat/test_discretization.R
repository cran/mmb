library(testthat)

source("../helpers.R")

test_that("we get warnings for NaN data", {
  w <- mmb::getWarnings()
  mmb::setWarnings(TRUE)

  expect_warning({
    mmb::discretizeVariableToRanges(c(1,5,3,NA, 3,NA,5))
  }, "Data contains NAs")

  mmb::setWarnings(w)
})


test_that("open-end ranges work", {
  ranges <- discretizeVariableToRanges(
    data = c(1,2,3,4,5,6,7,8,9),
    openEndRanges = TRUE,
    numRanges = 3
  )

  # (min-max)/3 = 2.66, offset=1 -> 3.66, 6.33, etc.

  expect_lt(ranges[[1]][1], -1.79e+308)
  expect_equal(ranges[[1]][2], 3+2/3, tolerance = 1e-12)

  expect_equal(ranges[[2]][1], 3+2/3, tolerance = 1e-12)
  expect_equal(ranges[[2]][2], 6+1/3, tolerance = 1e-12)

  expect_equal(ranges[[3]][1], 6+1/3, tolerance = 1e-12)
  expect_gt(ranges[[3]][2], 1.79e+308)

})


test_that("closed-end ranges work", {
  # exclusive minimum, inclusive minimum -> (x,y]
  # However, the exclusive minimum is smaller than the minimum (by 1e-15)
  # in the data, so that it is not excluded.

  ranges <- discretizeVariableToRanges(
    data = c(1,2,3,4,5,6,7,8,9),
    openEndRanges = FALSE,
    numRanges = 3,
  )

  expect_equal(length(ranges), 3)

  r1 <- ranges[[1]]
  expect_equal(getRangeForDiscretizedValue(ranges, 1), 1)
  expect_true(expect_does_throw({
    getRangeForDiscretizedValue(ranges, .999999)
  }))

  expect_true(r1[1] < 1)
  expect_true(expect_does_not_throw({
    getRangeForDiscretizedValue(ranges, 9)
  }))
})


test_that("numRanges is not required", {
  # this is done like this: max(c(2, ceiling(log2(length(data))))),
  # so that the amount of ranges is >= 2

  ranges <- expect_warning(discretizeVariableToRanges(c(1)))

  expect_gte(length(ranges), 2)

  ranges <- discretizeVariableToRanges(c(1,2,3,4,5,6,7,8))

  expect_equal(length(ranges), 3)
})


test_that("warnings and errors work", {
  expect_does_throw(discretizeVariableToRanges(
    data = NULL
  ))

  expect_does_throw(discretizeVariableToRanges(
    data = NULL,
    exclMinVal = -5,
    inclMaxVal = 5
  ))

  expect_does_not_throw(discretizeVariableToRanges(
    data = NULL,
    exclMinVal = -5,
    inclMaxVal = 5,
    numRanges = 3
  ))
})



