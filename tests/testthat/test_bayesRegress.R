library(testthat)

source("../helpers.R")

library(datasets)
data("iris")


test_that("it fails for invalid arguments", {
  expect_does_throw({
    mmb::bayesRegress(df = data.frame(), features = data.frame(), targetCol = "foo", numBuckets = 1)
  })

  m <- mmb::getMessages()
  mmb::setMessages(TRUE)
  w <- mmb::getWarnings()
  mmb::setWarnings(FALSE)

  expect_message({
    mmb::bayesRegress(
      df = iris[1:100, ],
      features = mmb::sampleToBayesFeatures(iris[101, ], "Sepal.Length"),
      targetCol = "Sepal.Length", numBuckets = 2)
  }, "No explicit feature selection")

  mmb::setWarnings(w)
  mmb::setMessages(m)
})


test_that("we can also sample from the most likely range only", {
  w <- mmb::getWarnings()
  mmb::setWarnings(TRUE)

  res <- expect_warning({
    mmb::bayesRegress(
      df = iris[1:100, ],
      features = mmb::sampleToBayesFeatures(iris[101, ], "Petal.Width"),
      targetCol = "Petal.Width", selectedFeatureNames = c("Species", "Sepal.Length"),
      sampleFromAllBuckets = FALSE, numBuckets = NA)
  }, "Segmenting stopped prematurely") # don't matter here

  expect_false(is.nan(res))
  expect_gt(res, 0)
})


test_that("custom regressor errors are handled properly", {
  w <- mmb::getWarnings()
  mmb::setWarnings(TRUE)

  res <- expect_warning({
    mmb::bayesRegress(
      df = iris[1:5, ], # data is too scarce so that PDF will not work
      features = mmb::sampleToBayesFeatures(iris[101, ], "Petal.Width"),
      targetCol = "Petal.Width", selectedFeatureNames = c("Species", "Sepal.Length"),
      sampleFromAllBuckets = FALSE)
  }, "Regressor returned NaN")

  expect_true(is.nan(res))

  # Also test a failing regressor:
  res <- expect_warning({
    mmb::bayesRegress(
      df = iris[1:100, ],
      features = mmb::sampleToBayesFeatures(iris[101, ], "Petal.Width"),
      targetCol = "Petal.Width", selectedFeatureNames = c("Species", "Sepal.Length"),
      sampleFromAllBuckets = FALSE, regressor = function(vec) stop("--42--"))
  }, "--42--")

  expect_true(is.nan(res))

  # Get the default regressor:
  rd <- mmb::getDefaultRegressor()
  mmb::setDefaultRegressor(function(data) paste(ceiling(data), collapse = "-"))
  res <- expect_warning({
    mmb::bayesRegress(
      df = iris[1:100, ],
      features = mmb::sampleToBayesFeatures(iris[101, ], "Petal.Width"),
      targetCol = "Petal.Width", selectedFeatureNames = c("Species", "Sepal.Length"),
      sampleFromAllBuckets = FALSE)
  }, "Segmenting stopped")
  expect_equal(grep("^\\d+?(\\-\\d+?)*?$", res), 1) # 42 or 42-1 or 54-4-12 and so on
  mmb::setDefaultRegressor(rd)

  mmb::setWarnings(w)
})


test_that("we can do regression for multiple values", {
  w <- mmb::getWarnings()
  mmb::setWarnings(FALSE)

  df <- iris[, ]
  set.seed(84735)
  rn <- base::sample(rownames(df), 150)
  dfTrain <- df[1:120, ]
  dfValid <- df[121:150, ]
  res <- mmb::bayesRegressAssign(
    dfTrain, dfValid[, !(colnames(dfValid) %in% "Sepal.Length")],
    "Sepal.Length", sampleFromAllBuckets = TRUE, doEcdf = TRUE)

  c <- cov(res, iris[121:150,]$Sepal.Length)^2
  expect_true(all(c >= 0) && all(c <= 1))

  mmb::setWarnings(w)
})


test_that("regression for multiple values works in simple and online, too", {
  w <- mmb::getWarnings()
  mmb::setWarnings(FALSE)

  df <- iris[, ]
  set.seed(84735)
  rn <- base::sample(rownames(df), 150)
  dfTrain <- df[1:120, ]
  dfValid <- df[121:150, ]

  res <- mmb::bayesRegressAssign(
    dfTrain, dfValid[, !(colnames(dfValid) %in% "Sepal.Length")],
    "Sepal.Length", sampleFromAllBuckets = FALSE, doEcdf = FALSE, online = 100, numBuckets = NA)

  c <- cov(res, iris[121:150,]$Sepal.Length)^2
  expect_true(all(c >= 0) && all(c <= 1))

  res <- mmb::bayesRegressAssign(
    dfTrain, dfValid[, ],
    "Sepal.Length", sampleFromAllBuckets = TRUE, doEcdf = FALSE, simple = TRUE)

  c <- cov(res, iris[121:150,]$Sepal.Length)^2
  expect_true(all(c >= 0) && all(c <= 1))

  mmb::setWarnings(w)
})
