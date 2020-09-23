library(testthat)

source("../helpers.R")

library(datasets)
data("iris")


test_that("invalid arguments in simple bayes lead to warn/stop", {
  expect_does_throw({
    mmb::bayesInferSimple(df = NULL, features = NULL, "bla")
  })
  expect_does_throw({
    mmb::bayesInferSimple(df = iris, features = NULL, "foo")
  })
  expect_does_throw({
    mmb::bayesInferSimple(df = iris[0,], features = data.frame(), "foo")
  })

  features <- sampleToBayesFeatures(iris[1,], "Species")

  expect_does_throw({
    mmb::bayesInferSimple(df = iris[0,], features, "Species")
  })

  expect_does_throw({
    mmb::bayesInferSimple(iris, features, "SpeciesXX")
  })

  w <- mmb::getWarnings()
  mmb::setWarnings(TRUE)

  expect_warning({
    mmb::bayesInferSimple(iris[1,], features, "Species")
  })

  expect_warning({
    featPet <- mmb::createFeatureForBayes("Petal.Length", mean(iris$Petal.Length))
    featSpe <- mmb::createFeatureForBayes("Species", iris$Species[1], isLabel = TRUE)
    mmb::bayesInferSimple(
      iris, rbind(featPet, featSpe), featSpe$name, retainMinValues = 1, doRegress = TRUE)
  })

  expect_warning({
    featSpe <- mmb::createFeatureForBayes("Species", iris$Species[1])
    res <- mmb::bayesInferSimple(
      iris, featSpe, "Petal.Length", retainMinValues = 0, doRegress = TRUE)
  })

  mmb::setWarnings(w)


  m <- mmb::getMessages()

  mmb::setMessages(TRUE)
  expect_message({
    featSpe <- mmb::createFeatureForBayes("Species", iris$Species[1])
    res <- mmb::bayesInferSimple(
      iris, featSpe, "Petal.Length", retainMinValues = 2, doRegress = TRUE)
  })

  expect_message({
    featPet <- mmb::createFeatureForBayes("Petal.Length", mean(iris$Petal.Length))
    featSpe <- mmb::createFeatureForBayes("Species", iris$Species[1], isLabel = TRUE)

    res <- mmb::bayesInferSimple(
      iris, rbind(featPet, featSpe), targetCol = featSpe$name,
      retainMinValues = 0, selectedFeatureNames = featPet$name)
  })

  mmb::setMessages(m)

  expect_does_throw({
    # Two labels, Species and Petal.Length will be auto generated.
    featSpe <- mmb::createFeatureForBayes("Species", iris$Species[1], isLabel = TRUE)
    res <- mmb::bayesInferSimple(iris, featSpe, "Petal.Length", doRegress = TRUE)
  })

  expect_does_throw({
    mmb::bayesInferSimple(df = c(1,2,3), data.frame(), "Petal.Length", doRegress = TRUE)
  })

  expect_does_throw({
    mmb::bayesInferSimple(iris, data.frame(), "Petal.Length", doRegress = TRUE)
  })

  expect_does_throw({
    mmb::bayesInferSimple(df = NULL, features = NULL, targetCol = NULL,
                          doRegress = TRUE, doEcdf = TRUE)
  })

  expect_does_throw({
    mmb::bayesConvertData(df = 5)
  })
})


test_that("a warning for ordered factors is issued", {
  w <- mmb::getWarnings()
  mmb::setWarnings(TRUE)

  fac <- factor(c("A", "B"), levels = c("A", "B"), ordered = TRUE)
  expect_warning({
    mmb::bayesConvertData(data.frame(bla = fac))
  })

  mmb::setWarnings(w)
})


test_that("we can do simple Bayesian inferencing", {
  temp <- iris[iris$Sepal.Length <= 6.1, ]
  cnt <- table(temp$Species)

  labels <- levels(iris$Species)
  featureSep <- mmb::createFeatureForBayes("Sepal.Length", 6.1)

  for (label in labels) {
    featureLab <- mmb::createFeatureForBayes(
      "Species", factor(label, levels = labels), isLabel = TRUE)

    # The probability should be proportional to the results in the above table
    res <- mmb::bayesProbabilitySimple(
      iris, rbind(featureLab, featureSep), "Species", selectedFeatureNames = "Sepal.Length")

    expect_equal(res, cnt[[label]] / sum(cnt), epsilon = 1e-16)
  }
})


test_that("we can do simple Bayesian inferencing of continuous values", {

  temp <- iris[iris$Species == iris$Species[1] & iris$Petal.Length <= 1.25, ]

  featSpe <- mmb::createFeatureForBayes("Species", iris$Species[1])
  featPet <- mmb::createFeatureForBayes("Petal.Length", 1.25)
  # We want to check how likely this value is given the conditional features,
  # if we plot the conditional density, the most likely value is around ~4.6,
  # so we test 4.6, and that 4 and 5 have a lower relative likelihood
  featSep <- mmb::createFeatureForBayes("Sepal.Length", 0, isLabel = TRUE)

  featSep$valueNumeric <- 4.6
  res_4_6 <- mmb::bayesProbabilitySimple(temp, rbind(
    featSpe, featPet, featSep
  ), targetCol = featSep$name, retainMinValues = 2)
  expect_gt(res_4_6, 0.6)
  expect_lt(res_4_6, 0.7)

  featSep$valueNumeric <- 4.0
  res_4_0 <- mmb::bayesProbabilitySimple(temp, rbind(
    featSpe, featPet, featSep
  ), targetCol = featSep$name, retainMinValues = 2)
  expect_gt(res_4_0, 0.2)
  expect_lt(res_4_0, 0.3)

  featSep$valueNumeric <- 5.0
  res_5_0 <- mmb::bayesProbabilitySimple(temp, rbind(
    featSpe, featPet, featSep
  ), targetCol = featSep$name, retainMinValues = 2)
  expect_gt(res_5_0, 0.4)
  expect_lt(res_5_0, 0.5)
})


test_that("we can inference probabilities on continuous values", {

  temp <- iris[iris$Species == iris$Species[1] & iris$Petal.Length <= 1.25, ]

  featSpe <- mmb::createFeatureForBayes("Species", iris$Species[1])
  featPet <- mmb::createFeatureForBayes("Petal.Length", 1.25)
  # Now to test the ecdf using these same conditions, we can see
  # that the probability of finding a value for Sepal.Length of
  # 4.6 or smaller is 50%
  featSep <- mmb::createFeatureForBayes("Sepal.Length", 4.6, isLabel = TRUE)

  res <- mmb::bayesProbabilitySimple(temp, rbind(
    featSpe, featPet, featSep
  ), targetCol = featSep$name, doEcdf = TRUE)
  expect_equal(res, 0.5, epsilon = 1e-12)
})


test_that("probability is zero if constraints too tight", {
  # Only 5 setosa remain
  temp <- iris[iris$Sepal.Length <= 4.5, ]
  featureSep <- mmb::createFeatureForBayes("Sepal.Length", 4.5)

  labelProbs <- list(
    setosa = 1,
    versicolor = 0,
    virginica = 0
  )

  labels <- levels(iris$Species)
  for (label in labels) {
    featureLab <- mmb::createFeatureForBayes(
      "Species", factor(label, levels = labels), isLabel = TRUE)

    res <- mmb::bayesProbabilitySimple(
      iris, rbind(featureLab, featureSep), "Species", selectedFeatureNames = "Sepal.Length")

    expect_equal(res, labelProbs[[label]], epsilon = 1e-16)
  }
})


test_that("simple Bayesian regression using one or more features work", {
  temp <- iris[iris$Sepal.Length <= 6.1 & iris$Sepal.Width <= 2.9, ]

  featLen <- mmb::createFeatureForBayes("Sepal.Length", 6.1)
  featWid <- mmb::createFeatureForBayes("Sepal.Width", 2.9)

  res <- mmb::bayesRegressSimple(iris, rbind(
    featLen,
    featWid
  ), selectedFeatureNames = c(), targetCol = "Petal.Length")

  expect_does_throw({
    mmb::bayesRegressSimple(iris, rbind(
      featLen,
      featWid
    ), retainMinValues = 1, selectedFeatureNames = c(), targetCol = "Petal.Length")
  })

  # The regression builds the PDF over the remaining Petal.Length and returns argmax
  pdf <- stats::density(temp$Petal.Length, bw = "SJ")

  expect_equal(res, pdf$x[which.max(pdf$y)], epsilon = 1e-15)
})


test_that("an error is thrown if there is a discrepancy in target-features", {
  w <- mmb::getWarnings()
  mmb::setWarnings(FALSE)

  feat1 <- mmb::createFeatureForBayes(
    name = "Sepal.Length", value = mean(iris$Sepal.Length))
  feat2 <- mmb::createFeatureForBayes(
    name = "Sepal.Width", value = mean(iris$Sepal.Width), isLabel = TRUE)

  expect_does_throw({
    mmb::bayesProbabilitySimple(df = iris, features = rbind(feat1, feat2), targetCol = feat1$name)
  })
  expect_does_not_throw({
    mmb::bayesProbabilitySimple(df = iris, features = rbind(feat1, feat2), targetCol = feat2$name)
  })

  mmb::setWarnings(w)
})


test_that("one features as designated label is always required", {
  w <- mmb::getWarnings()
  mmb::setWarnings(FALSE)

  feat1 <- mmb::createFeatureForBayes(
    name = "Sepal.Length", value = mean(iris$Sepal.Length), isLabel = FALSE)
  feat2 <- mmb::createFeatureForBayes(
    name = "Sepal.Width", value = mean(iris$Sepal.Width), isLabel = FALSE)

  expect_does_throw({
    mmb::bayesProbabilitySimple(df = iris, features = rbind(feat1, feat2), targetCol = feat1$name)
  })

  mmb::setWarnings(w)
})

