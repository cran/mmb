library(testthat)

source("../helpers.R")

library(datasets)
data("iris")


test_that("we can obtain a neighborhood for any sample", {
  set.seed(84735)
  n <- sample(rownames(iris), 10)
  q <- setdiff(rownames(iris), n)

  w <- mmb::getWarnings()
  mmb::setWarnings(FALSE)

  for (x in n) {
    s <- mmb::sampleToBayesFeatures(iris[x, ], "Species")
    dfN <- mmb::neighborhood(iris, s)

    # The "comb" should be in it, too
    expect_true(x %in% rownames(dfN))
    # .. but usally not all other rows..
    expect_gt(length(setdiff(rownames(iris), rownames(dfN))), 0)
  }

  mmb::setWarnings(w)
})


test_that("we get errors and warnins for centralities", {
  w <- mmb::getWarnings()
  mmb::setWarnings(TRUE)

  expect_does_throw({
    mmb::centralities(dfNeighborhood = c())
  })

  expect_warning({
    mmb::centralities(iris)
  }, "No explicit feature")

  expect_warning({
    mmb::centralities(iris[0, ], selectedFeatureNames = c("Species"))
  }, "dfNeighborhood is empty")

  expect_warning({
    mmb::centralities(iris, selectedFeatureNames = "Species", doEcdf = FALSE, ecdfMinusOne = TRUE)
  }, "ecdfMinusOne")

  mmb::setWarnings(w)
})


test_that("we can obtain centralities", {
  set.seed(84735)

  for (n in 1:10) {
    temp <- iris[sample(rownames(iris), 50), ]
    c <- mmb::centralities(temp, selectedFeatureNames = colnames(iris), shiftAmount = 0, doEcdf = TRUE)

    expect_equal(length(setdiff(rownames(temp), names(c))), 0)
    expect_true(sum(c < 0) + sum(c > 1) == 0)
  }
})


test_that("getting vicinities for one sample works", {
  expect_does_throw({
    mmb::vicinitiesForSample(df = c(), sampleFromDf = iris[1,])
  })
  expect_does_throw({
    mmb::vicinitiesForSample(iris, sampleFromDf = c())
  })
  expect_does_throw({
    mmb::vicinitiesForSample(iris, sampleFromDf = iris[0, ])
  })

  w <- mmb::getWarnings()
  mmb::setWarnings(TRUE)

  expect_warning({
    mmb::vicinitiesForSample(iris, sampleFromDf = iris[1:2, ])
  }, "More than one")

  mmb::setWarnings(w)
})


test_that("we can obtain the distance between 2 samples", {
  w <- mmb::getWarnings()
  mmb::setWarnings(FALSE)

  d <- mmb::distance(
    dfNeighborhood = iris, rowNrOfSample1 = "1", rowNrOfSample2 = "20"
  )
  expect_gte(d, 0)
  mmb::setWarnings(w)
})


test_that("we can obtain the entire matrix of vicinities", {
  m <- mmb::getMessages()
  mmb::setMessages(TRUE)
  w <- mmb::getWarnings()
  mmb::setWarnings(FALSE)

  l <- nrow(iris)
  vic <- expect_message({
    mmb::vicinities(iris, useParallel = FALSE)
  }, "Using sequential")

  expect_equal(length(vic), l^2)
  # In the resulting matrix, the represents the i-th neighborhood,
  # and each column is the vicinity of sample i to this neighborhood.
  # Therefore, vic_i,j is always greater 0
  expect_equal(sum(diag(vic) == 0), 0)

  cl <- parallel::makePSOCKcluster(2)
  doParallel::registerDoParallel(cl)
  vicPar <- tryCatch({
    expect_message({
      mmb::vicinities(iris, useParallel = TRUE)
    }, "Using registered parallel backend")
  }, finally = {
    parallel::stopCluster(cl)
    doParallel::stopImplicitCluster()
    foreach::registerDoSEQ()
  })

  expect_equal(vic, vicPar, tolerance = 1e-10)

  # Check special cases:
  expect_does_throw({
    mmb::vicinities(df = c(), useParallel = FALSE)
  })
  expect_does_throw({
    mmb::vicinities(df = iris[0, ], useParallel = FALSE)
  })
  expect_does_not_throw({
    mmb::vicinities(df = iris[5, ], useParallel = FALSE)
  })

  mmb::setMessages(m)
  mmb::setWarnings(w)
})



