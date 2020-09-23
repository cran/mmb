library(testthat)

source("../helpers.R")

library(datasets)
data("iris")


test_that("we get messages from Naive Bayes", {
  m <- mmb::getMessages()
  mmb::setMessages(TRUE)

  expect_message({
    mmb::bayesProbabilityNaive(
      df = iris,
      features = mmb::sampleToBayesFeatures(iris[42, ], targetCol = "Species"),
      targetCol = "Species")
  }, "No explicit feature selection")

  cl <- parallel::makePSOCKcluster(2)
  doParallel::registerDoParallel(cl)
  expect_true(foreach::getDoParRegistered())

  expect_message({
    mmb::bayesProbabilityNaive(
      df = iris, features = mmb::sampleToBayesFeatures(
      iris[42, ], targetCol = "Species"),
      targetCol = "Species", selectedFeatureNames = colnames(iris)[2:4],
      useParallel = TRUE)
  }, "Using registered parallel")

  parallel::stopCluster(cl)
  doParallel::stopImplicitCluster()
  foreach::registerDoSEQ()

  #expect_false(foreach::getDoParRegistered()) # not sure how to check this atm
  expect_message({
    mmb::bayesProbabilityNaive(
      df = iris, features = mmb::sampleToBayesFeatures(
        iris[42, ], targetCol = "Species"),
      targetCol = "Species", selectedFeatureNames = colnames(iris)[2:4],
      useParallel = FALSE)
      #useParallel = TRUE) # not registered actually
  }, "Using sequential")

  mmb::setMessages(m)
})
