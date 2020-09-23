library(testthat)

source("../helpers.R")


test_that("estimation for small data works", {
  # these are special cases

  w <- mmb::getWarnings()

  est <- expect_warning({
    mmb::estimatePdf(c())
  })

  expect_equal(est$min, 0)
  expect_equal(est$max, 0)
  expect_equal(est$fun(123543.245), 0) # Any value should be 0

  est <- expect_warning({
    mmb::estimatePdf(c(1337))
  })

  expect_equal(est$fun(1336), 0) # all other values should be 0
  expect_equal(est$fun(1337), 1)
  expect_gte(est$min, 1337 - 1e-16)
  expect_lte(est$max, 1337 + 1e-16)

  mmb::setWarnings(w)
})


test_that("overriding the density function works", {
  expect_does_not_throw({ # also, no warnings either
    est <- mmb::estimatePdf(c(1,2,3), stats::density)
    expect_lt(abs(1/3 - est$fun(2)), 1e-2)
    est <- mmb::estimatePdf(c(1,2,3), function(x) stats::density(x, bw = "nrd0"))
    expect_lt(abs(1/3 - est$fun(2)), 1e-2)
  })

  # let's test the default function
  est <- mmb::estimatePdf(c(1,2,3))
  expect_lt(abs(1/3 - est$fun(2)), 2e-2)
})


test_that("errors are handled as warnings when estimating density", {
  w <- mmb::getWarnings()
  mmb::setWarnings(TRUE)

  expect_warning({
    mmb::estimatePdf(c(1,1))
  }, "Density estimation failed")

  mmb::setWarnings(w)
})


test_that("probability for discrete is correct", {
  w <- mmb::getWarnings()
  mmb::setWarnings(TRUE)

  expect_warning({
    mmb::getProbForDiscrete(c(), "foo")
  })

  mmb::setWarnings(w)

  # Each count is the same
  expect_equal(mmb::getProbForDiscrete(iris$Species, iris$Species[1]), 1/3)
})



