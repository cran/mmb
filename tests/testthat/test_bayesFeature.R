library(testthat)

source("../helpers.R")

library(datasets)
data("iris")


test_that("transformation to sample fails for invalid data", {
  expect_does_throw({
    mmb::sampleToBayesFeatures(data.frame(), "bla")
  })

  expect_does_throw({
    mmb::sampleToBayesFeatures(iris, 43)
  })

  expect_does_throw({
    mmb::sampleToBayesFeatures(iris, "SPEC") # not contained
  })
})


test_that("corrupted features lead to errors", {
  feat <- mmb::createFeatureForBayes("foo", NA)
  # Erroneous manipulation (NA is logical)
  feat$isLogical <- F

  expect_does_throw({
    mmb::getValueOfBayesFeatures(feat, feat$name)
  })

  expect_does_throw({
    mmb::getValueKeyOfBayesFeatures(feat, feat$name)
  })
})


test_that("a sample can be transformed to a bayesian feature (and back)", {
  df <- iris

  s <- mmb::sampleToBayesFeatures(df[1,], "Species")
  s1 <- s[1,]
  expect_equal(s1$name, "Species")
  expect_true(s1$isLabel)
  expect_true(s1$isDiscrete)
  expect_true(is.character(s1$valueChar))
  expect_true(is.na(s1$valueNumeric))
  expect_true(is.na(s1$valueBool))

  featNames <- s$name[2:5]
  expect_equal(nrow(s), 5) # 4 features + label

  # all features are numeric:
  for (fn in featNames) {
    expect_equal(mmb::getValueKeyOfBayesFeatures(s, fn), "valueNumeric")
  }

  bf <- mmb::bayesFeaturesToSample(iris[1, ], s)
})


test_that("invalid arguments for back-transformation will throw", {
  expect_does_throw({
    mmb::bayesFeaturesToSample(dfOrg = NULL, data.frame())
  })
  expect_does_throw({
    mmb::bayesFeaturesToSample(dfOrg = data.frame(), features = NULL)
  })
  expect_does_throw({
    mmb::bayesFeaturesToSample(dfOrg = data.frame(), data.frame())
  })
})


test_that("a sample can have numeric, string, factor and bool features", {
  df <- data.frame(
    num = c(1.42, 1337),
    fac = as.factor(c("X1", "X2")),
    str = c("foo", "bar"),
    bol = c(T, F)
  )
  df$str <- as.character(df$str)

  s <- mmb::sampleToBayesFeatures(df[2,], "fac")

  expect_equal(mmb::getValueKeyOfBayesFeatures(s, "num"), "valueNumeric")
  expect_equal(mmb::getValueKeyOfBayesFeatures(s, "fac"), "valueChar")
  expect_equal(mmb::getValueKeyOfBayesFeatures(s, "str"), "valueChar")
  expect_equal(mmb::getValueKeyOfBayesFeatures(s, "bol"), "valueBool")

  expect_equal(mmb::getValueOfBayesFeatures(s, "num"), 1337)
  expect_equal(mmb::getValueOfBayesFeatures(s, "fac"), "X2")
  expect_equal(mmb::getValueOfBayesFeatures(s, "str"), "bar")
  expect_equal(mmb::getValueOfBayesFeatures(s, "bol"), F)
})


test_that("a sample may not have other kinds of values", {
  expect_does_throw({
    mmb::getValueOfBayesFeatures(dfFeature = list(bla=5), "bla")
  })
  expect_does_throw({
    mmb::getValueOfBayesFeatures(iris, "SPC")
  })

  df <- data.frame(
    num = c(1.42, 1337),
    fac = as.factor(c("X1", "X2")),
    str = c("foo", "bar"),
    bol = c(T, F)
  )
  df$str <- as.character(df$str)

  s <- mmb::sampleToBayesFeatures(df[2,], "fac")
  # Let's set all to NA
  s$valueNumeric <- NA
  s$valueFactor <- NA
  s$valueString <- NA
  s$valueBool <- NA

  expect_does_throw({
    mmb::getValueOfBayesFeatures(s, "num")
  })

  expect_does_throw({
    mmb::getValueKeyOfBayesFeatures(s, "num")
  })
})


test_that("it stops if arguments are invalid", {
  expect_does_throw({
    checkBayesFeature(data.frame(foo=42), featName = "")
  })
})




