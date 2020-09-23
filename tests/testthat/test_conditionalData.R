library(testthat)

source("../helpers.R")

library(datasets)
data("iris")


test_that("conditioning does nothing if called wrong", {
  w <- mmb::getWarnings()
  mmb::setWarnings(TRUE)

  expect_warning({
    mmb::conditionalDataMin(data.frame())
  })

  expect_warning({
    mmb::conditionalDataMin(data.frame(bla=c(1,2)))
  })

  mmb::setWarnings(w)
})


test_that("conditioning on one variable works", {
  cntSetosa <- nrow(iris[iris$Species == iris[1,]$Species, ])
  fDf <- mmb::createFeatureForBayes(
    "Species", iris[1,]$Species, isLabel = TRUE, isDiscrete = TRUE)

  dfRes <- mmb::conditionalDataMin(iris, fDf, selectedFeatureNames = c("Species"))

  expect_equal(nrow(dfRes), cntSetosa)


  cntSep5 <- nrow(iris[iris$Sepal.Length <= 5, ])
  fdF <- mmb::createFeatureForBayes("Sepal.Length", 5)

  dfRes <- mmb::conditionalDataMin(iris, fdF, selectedFeatureNames = c("Sepal.Length"))

  expect_equal(nrow(dfRes), cntSep5)
})


test_that("conditioning on more than one variable works", {
  cntIris <- nrow(iris[iris$Species == iris[100,]$Species & iris$Sepal.Length <= 5, ])
  featSp <- mmb::createFeatureForBayes("Species", iris[100,]$Species)
  featSl <- mmb::createFeatureForBayes("Sepal.Length", 5)

  dfRes <- mmb::conditionalDataMin(
    iris,
    rbind(featSp, featSl),
    selectedFeatureNames = c(featSp$name, featSl$name))

  expect_equal(nrow(dfRes), cntIris)
})


test_that("conditioning stops early", {
  # In this test we ascertain that segmenting stop when a
  # threshold is undercut.

  cntIris <- nrow(iris[iris$Species == iris[1,]$Species & iris$Sepal.Length <= 5, ])
  # The following count is larger because less conditions apply
  cntIrisNoSepal <- nrow(iris[iris$Species == iris[1,]$Species, ])
  expect_gt(cntIrisNoSepal, cntIris)

  featSp <- mmb::createFeatureForBayes("Species", iris[1,]$Species)
  featSl <- mmb::createFeatureForBayes("Sepal.Length", 5)

  dfRes <- mmb::conditionalDataMin(
    iris,
    rbind(featSp, featSl),
    selectedFeatureNames = c(featSp$name, featSl$name)
  )

  w <- mmb::getWarnings()
  mmb::setWarnings(TRUE)
  dfResMin <- expect_warning({
    mmb::conditionalDataMin(
      iris,
      rbind(featSp, featSl),
      selectedFeatureNames = c(featSp$name, featSl$name),
      retainMinValues = cntIris + 1 # that should lead to skipping Sepal.Length
    )
  })

  expect_true(nrow(dfRes) < nrow(dfResMin)) # retain more than cntIris
  expect_equal(nrow(dfRes), cntIris)
  expect_equal(nrow(dfResMin), cntIrisNoSepal)

  mmb::setWarnings(w)
})


test_that("handling of empty values is proper", {
  df <- data.frame(
    na = c( T, T, F,   F,   NA, NA),
    nan = c(1, 4, NaN, NaN, 3,  2)
  )

  feat <- mmb::createFeatureForBayes("nan", NaN)

  # should retain rows 3,4
  data <- mmb::conditionalDataMin(df, feat, selectedFeatureNames = "nan")
  expect_equal(nrow(data), 2)
  expect_equal(data$na, c(F,F))

  feat <- mmb::createFeatureForBayes("na", NA)

  data <- mmb::conditionalDataMin(df, feat, selectedFeatureNames = "na")
  expect_equal(nrow(data), 2)
  expect_equal(data$nan, c(3,2))
})


test_that("rownames are preserved", {
  df <- mmb::bayesConvertData(iris)
  temp <- df[(df$Species %in% df$Species[2]) &
               df$Sepal.Length <= mean(df$Sepal.Length), ]

  tempCdm <- mmb::conditionalDataMin(df, rbind(
    mmb::createFeatureForBayes("Species", df$Species[2]),
    mmb::createFeatureForBayes("Sepal.Length", mean(df$Sepal.Length))
  ), selectedFeatureNames = c("Species", "Sepal.Length"))

  res <- setdiff(rownames(temp), rownames(tempCdm))
  expect_true(is.character(res))
  expect_equal(length(res), 0)
})



