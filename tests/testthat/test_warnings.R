library(testthat)


test_that("closures over variables work", {
  # This test is required explicitly
  vc <- make.varClosure(42)

  expect_equal(vc$get(), 42)
  vc$set(1337)
  expect_equal(vc$get(), 1337)
})


test_that("en-/disabling warnings/errors works", {
  # Check defaults
  expect_true(mmb::getWarnings())

  mmb::setWarnings(FALSE)

  expect_false(mmb::getWarnings())
})


test_that("a custom validator can be used", {
  vc <- make.varClosure(valValidator = is.logical)

  expect_does_throw({
    vc$set(42)
  })
  expect_does_not_throw({
    expect_true(is.null(vc$get()))
    vc$set(TRUE)
    expect_true(vc$get())
  })
})
