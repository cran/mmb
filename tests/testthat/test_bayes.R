library(testthat)

source("../helpers.R")

library(datasets)
data("iris")


getNumCores <- function() {
  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")

  if (nzchar(chk) && chk == "TRUE") {
    # use 2 cores in CRAN/Travis/AppVeyor
    return(2)
    num_workers <- 2L
  }
  # use all cores in devtools::test()
  return(num_workers <- parallel::detectCores())
}


test_that("it fails for invalid arguments", {
  expect_does_throw({
    mmb::bayesProbabilityAssign(
      dfTrain = iris, dfValid = iris, targetCol = "Species",
      simple = TRUE, naive = TRUE) # can't do both
  })
})


test_that("the factors for products are built correctly", {
  df <- data.frame(
    A = iris$Species,
    B = iris$Sepal.Length,
    C = iris$Petal.Width
  )

  cf <- rbind(
    mmb::createFeatureForBayes("B", mean(df$B)),
    mmb::createFeatureForBayes("C", mean(df$C))
  )

  tf <- mmb::createFeatureForBayes("A", df$A[1], isLabel = TRUE)

  w <- mmb::getWarnings()
  mmb::setWarnings(TRUE)
  m <- mmb::getMessages()
  mmb::setMessages(TRUE)

  expect_does_throw({
    bayesComputeProductFactor(df, data.frame(), tf, computeNumerator = TRUE)
  })
  expect_does_throw({
    bayesComputeProductFactor(df, cf, data.frame(), computeNumerator = TRUE)
  })
  expect_does_not_throw({
    expect_message({
      bayesComputeProductFactor(df, cf, data.frame(), computeNumerator = FALSE)
    })
  })
  expect_warning({
    bayesComputeProductFactor(df, tf, cf, computeNumerator = TRUE)
  })

  # Let's filter iris and let B's dist. depend on C,A
  filtered <- df[df$C <= mean(df$C) & df$A == df$A[1], ]
  # Emp. PDF of B, segmented on C=c, A=a
  dens <- approxfun(stats::density(filtered$B, bw = "SJ"))
  # Returns P(B|C,A)
  bayesRegressVal <- bayesComputeProductFactor(df, cf, tf, computeNumerator = TRUE)

  expect_equal(bayesRegressVal, dens(cf$valueNumeric[1]), tolerance = 1e-15)


  # Ask for a value out of range for B:
  cf[1,]$valueNumeric <- 0.1
  expect_equal(bayesComputeProductFactor(df, cf, tf, computeNumerator = TRUE), 0)

  mmb::setWarnings(w)
  mmb::setMessages(m)
})


test_that("using the ecdf works as well", {
  df <- data.frame(
    A = iris$Species,
    B = iris$Sepal.Length,
    C = iris$Petal.Width
  )

  tf <- mmb::createFeatureForBayes("A", df$A[1], isLabel = TRUE)
  cf <- mmb::createFeatureForBayes("B", mean(df$B))

  temp <- df[df$A == df$A[1],]
  res <- bayesComputeProductFactor(df, cf, tf, computeNumerator = TRUE, doEcdf = TRUE)
  expect_equal(res, ecdf(temp$B)(cf$valueNumeric), tolerance = 1e-10)

  # Let's do too aggressive segmenting
  cf <- rbind(cf, mmb::createFeatureForBayes("C", min(df$C - 1e-10)))
  res <- expect_warning({
    bayesComputeProductFactor(
      df, cf, tf, computeNumerator = TRUE,
      doEcdf = TRUE, retainMinValues = 0)
  })
  expect_equal(res, 0)
})


test_that("marginal factors are calculated correctly", {
  expect_equal(mmb::bayesComputeMarginalFactor(
    iris, mmb::createFeatureForBayes("Species", iris$Species[1])), 1/3, tolerance = 1e-10)

  w <- mmb::getWarnings()
  mmb::setWarnings(TRUE)

  expect_warning({
    # ecdf has no effect for discrete
    mmb::bayesComputeMarginalFactor(
      iris, mmb::createFeatureForBayes("Species", iris$Species[1]), doEcdf = TRUE)
  })

  expect_warning({
    # no data
    mmb::bayesComputeMarginalFactor(
      iris[0,], mmb::createFeatureForBayes("Petal.Length", 1337), doEcdf = TRUE)
  })

  mmb::setWarnings(w)


  # Let's do some things that should definitely work!
  df <- data.frame(
    data = rnorm(25000)
  )

  res <- mmb::bayesComputeMarginalFactor(
    df, mmb::createFeatureForBayes("data", 0), doEcdf = TRUE)

  expect_equal(res, 0.5, tolerance = 1e-2)

  res <- mmb::bayesComputeMarginalFactor(
    df, mmb::createFeatureForBayes("data", 0), doEcdf = FALSE)

  expect_equal(
    res, stats::approxfun(stats::density(df$data, bw = "SJ"))(0), tolerance = 1e-15)
})


test_that("the full Bayesian works for discrete, serial and parallel", {
  if (base::Sys.getenv("IS_BUILD_COMMAND") != "TRUE") {
    # disable this test for cmd+shift+T
    expect_true(TRUE) # dummy
    return(TRUE) # skip test
  }

  # In this test, we are doing a fully-fledged example of
  #
  # P(A|B) = P(B|A) * P(A) / P(B)
  #
  # using the Iris dataset. We will compute each factor single handedly,
  # then compare the results. Then we check PDF vs. CDF and also
  # parallel computation vs. serial.

  P <- function(data, val) mmb::getProbForDiscrete(data, val)
  PDF <- function(data, val) stats::approxfun(stats::density(data, bw="SJ"))(val)
  CDF <- function(data, val) stats::ecdf(data)(val)

  A <- iris$Species
  a <- A[1]
  B <- iris$Sepal.Length
  b <- mean(B)

  df <- data.frame(A=A, B=B)
  feats <- rbind(
    mmb::createFeatureForBayes("A", a, isLabel = TRUE),
    mmb::createFeatureForBayes("B", b)
  )

  # P(B|A) as PDF,CDF
  temp <- df[df$A == a, ]
  p_BA_pdf <- PDF(temp$B, b)
  p_BA_cdf <- CDF(temp$B, b)
  # P(A)
  p_A <- P(A, a)
  # P(B) as PDF,CDF
  p_B_pdf <- PDF(B, b)
  p_B_cdf <- CDF(B, b)
  # P(A|B) as PDF,CDF
  p_AB_pdf <- p_BA_pdf * p_A / p_B_pdf
  p_AB_cdf <- p_BA_cdf * p_A / p_B_cdf


  res <- mmb::bayesProbability(
    df, feats, "A", shiftAmount = 0, doEcdf = FALSE, useParallel = FALSE)
  expect_equal(res, p_AB_pdf, tolerance = 1e-15)

  res <- mmb::bayesProbability(
    df, feats, "A", shiftAmount = 0, doEcdf = TRUE, useParallel = FALSE)
  expect_equal(res, p_AB_cdf, tolerance = 1e-15)


  # Let's test shifting!
  s <- .25
  p_AB_pdf_s <- (p_BA_pdf + s) * (p_A + s) / (p_B_pdf + s)
  p_AB_cdf_s <- (p_BA_cdf + s) * (p_A + s) / (p_B_cdf + s)

  res <- mmb::bayesProbability(
    df, feats, "A", shiftAmount = s, doEcdf = FALSE, useParallel = FALSE)
  expect_equal(res, p_AB_pdf_s, tolerance = 1e-15)

  m <- mmb::getMessages()
  mmb::setMessages(TRUE)
  res <- expect_message({
    # using seq. comp.
    mmb::bayesProbability(
      df, feats, "A", shiftAmount = s, doEcdf = TRUE, useParallel = FALSE)
  })
  expect_equal(res, p_AB_cdf_s, tolerance = 1e-15)
  mmb::setMessages(m)


  # Let's test parallel!
  cl <- parallel::makePSOCKcluster(getNumCores())
  doParallel::registerDoParallel(cl)
  expect_true(foreach::getDoParRegistered())

  tryCatch({
    res <- mmb::bayesProbability(
    df, feats, "A", shiftAmount = 0, doEcdf = FALSE, useParallel = TRUE)
    expect_equal(res, p_AB_pdf, tolerance = 1e-15)

    m <- mmb::getMessages()
    mmb::setMessages(TRUE)
    res <- expect_message({
      # no expl. feat. sel.
      mmb::bayesProbability(
        df, feats, "A", shiftAmount = 0, doEcdf = TRUE, useParallel = TRUE)
    }, "No explicit feature selection")

    expect_equal(res, p_AB_cdf, tolerance = 1e-15)

    res <- expect_message({
      # Using parallel
      browser()
      mmb::bayesProbability(
        df, feats, "A", useParallel = TRUE, selectedFeatureNames = c("B"))
    }, "Using registered parallel backend")

    mmb::setMessages(m)
  }, finally = {
    parallel::stopCluster(cl)
    doParallel::stopImplicitCluster()
    foreach::registerDoSEQ()
  })
})


test_that("the full Bayesian works for continuous", {
  # In this test, we are doing a fully-fledged example of
  #
  # P(A|B) = P(B|A) * P(A) / P(B)
  #
  # using the Iris dataset. We will compute each factor single handedly,
  # then compare the results. Then we check PDF vs. CDF..

  P <- function(data, val) mmb::getProbForDiscrete(data, val)
  PDF <- function(data, val) stats::approxfun(stats::density(data, bw="SJ"))(val)
  CDF <- function(data, val) stats::ecdf(data)(val)

  A <- iris$Sepal.Length
  a <- mean(A)
  B <- iris$Species
  b <- B[1]

  df <- data.frame(A=A, B=B)
  feats <- rbind(
    mmb::createFeatureForBayes("A", a, isLabel = TRUE),
    mmb::createFeatureForBayes("B", b)
  )

  # P(B|A)
  temp <- df[df$A <= a, ]
  p_BA <- P(temp$B, b)

  # P(A) as PDF,CDF
  p_A_pdf <- PDF(A, a)
  p_A_cdf <- CDF(A, a)

  # P(B)
  p_B <- P(B, b)

  # P(A|B) as PDF,CDF
  p_AB_pdf <- p_BA * p_A_pdf / p_B
  p_AB_cdf <- p_BA * p_A_cdf / p_B


  res <- mmb::bayesProbability(
    df, feats, "A", shiftAmount = 0, doEcdf = FALSE, useParallel = FALSE)
  expect_equal(res, p_AB_pdf, tolerance = 1e-15)

  res <- mmb::bayesProbability(
    df, feats, "A", shiftAmount = 0, doEcdf = TRUE, useParallel = FALSE)
  expect_equal(res, p_AB_cdf, tolerance = 1e-15)


  # Let's test shifting!
  s <- .25
  p_AB_pdf_s <- (p_BA + s) * (p_A_pdf + s) / (p_B + s)
  p_AB_cdf_s <- (p_BA + s) * (p_A_cdf + s) / (p_B + s)

  res <- mmb::bayesProbability(
    df, feats, "A", shiftAmount = s, doEcdf = FALSE, useParallel = FALSE)
  expect_equal(res, p_AB_pdf_s, tolerance = 1e-15)

  m <- mmb::getMessages()
  mmb::setMessages(TRUE)
  res <- expect_message({
    # using seq. comp.
    mmb::bayesProbability(
      df, feats, "A", shiftAmount = s, doEcdf = TRUE, useParallel = FALSE)
  })
  expect_equal(res, p_AB_cdf_s, tolerance = 1e-15)
  mmb::setMessages(m)
})


test_that("the full Bayesian works with many variables", {
  df <- mtcars[, ]
  # It's all num. feats., let's transform some
  df$cyl <- as.factor(df$cyl)
  df$carb <- as.factor(df$carb)
  tCol <- "cyl"

  set.seed(84735)
  rn <- base::sample(rownames(df), 32)
  dfTrain <- df[rn[1:25], ]
  # Let's take it out just to make sure.
  dfValid <- df[rn[26:32], !(colnames(df) %in% tCol) ]

  w <- mmb::getWarnings()
  mmb::setWarnings(FALSE)

  probs <- mmb::bayesProbabilityAssign(
    dfTrain, dfValid, targetCol = tCol,
    selectedFeatureNames = c("mpg", "hp", "carb"),
    shiftAmount = 0, doEcdf = TRUE, simple = FALSE, returnProbabilityTable = FALSE)

  expect_true(is.factor(probs))
  expect_equal(levels(probs), levels(dfTrain$cyl))

  mmb::setWarnings(w)

  # This should work w/o probs.
  cm <- caret::confusionMatrix(df[26:32,]$cyl, probs)
})


test_that("assigning probabilites for multiple values works", {
  set.seed(84735)
  rn <- base::sample(rownames(iris), 150)
  dfTrain <- iris[rn[1:120], ]
  # Let's take it out just to make sure.
  dfValid <- iris[rn[121:150], !(colnames(iris) %in% "Species") ]

  w <- mmb::getWarnings()
  mmb::setWarnings(FALSE)

  probTable <- mmb::bayesProbabilityAssign(
    dfTrain, dfValid, targetCol = "Species", shiftAmount = 0,
    doEcdf = TRUE, simple = FALSE, returnProbabilityTable = TRUE)

  mmb::setWarnings(w)

  # Since we use shift=0 and ecdf=T, each row in this table
  # should sum up to one, i.e., actual probs are returned.
  # For some corner cases, a row can also be zero; this happens
  # for when no other row (<=) can be found and we do not use
  # a positive shift.
  for (rn in rownames(probTable)) {
    temp <- probTable[rn, levels(iris$Species)]
    s <- sum(temp)
    if (s == 0) {
      expect_equal(s, 0)
    } else {
      expect_equal(s, 1, tolerance = 1e-14)
    }
  }
})


test_that("assigning probability for numeric values works (also using simple)", {
  set.seed(84735)
  rn <- base::sample(rownames(iris), 150)
  dfTrain <- iris[rn[1:120], ]
  dfValid <- iris[rn[121:150], ]

  w <- mmb::getWarnings()
  mmb::setWarnings(FALSE)

  probs <- mmb::bayesProbabilityAssign(
    dfTrain, dfValid, targetCol = "Sepal.Length", shiftAmount = 0,
    doEcdf = TRUE, simple = FALSE, returnProbabilityTable = FALSE)

  # Since we use shift=0 and ecdf=T, each value should be <= 1.
  # Note that because of rounding errors, we need to check this
  # using some tolerance.
  sapply(probs, function(p) expect_lte(p, 1+1e-15))

  probs <- mmb::bayesProbabilityAssign(
    dfTrain, dfValid, targetCol = "Sepal.Length", shiftAmount = 0,
    doEcdf = TRUE, simple = TRUE, returnProbabilityTable = FALSE)
  sapply(probs, function(p) expect_lte(p, 1+1e-15))

  mmb::setWarnings(w)
})


test_that("assigning for multiple works using naive Bayes", {
  set.seed(84735)
  rn <- base::sample(rownames(iris), 150)
  dfTrain <- iris[rn[1:120], ]
  dfValid <- iris[rn[121:150], ]

  w <- mmb::getWarnings()
  mmb::setWarnings(FALSE)

  probs <- mmb::bayesProbabilityAssign(
    dfTrain, dfValid, targetCol = "Species",
    doEcdf = TRUE, naive = TRUE, returnProbabilityTable = FALSE)

  cm <- caret::confusionMatrix(probs, dfValid$Species)
  expect_gt(cm[["overall"]][[1]], .5)

  mmb::setWarnings(w)
})


test_that("a numeric value is required for predicting its probability", {
  set.seed(84735)
  rn <- base::sample(rownames(iris), 150)
  dfTrain <- iris[1:120, ]
  # Let's take it out just to make sure.
  dfValid <- iris[121:150, !(colnames(iris) %in% "Sepal.Length") ]

  expect_does_throw({
    mmb::bayesProbabilityAssign(dfTrain, dfValid, targetCol = "Sepal.Length")
  })
})


test_that("we can do online learning and return factors", {
  set.seed(84735)
  rn <- base::sample(rownames(iris), 150)
  dfTrain <- iris[1:140, ]
  dfValid <- iris[141:150, ]

  w <- mmb::getWarnings()
  mmb::setWarnings(FALSE)

  pred <- mmb::bayesProbabilityAssign(
    dfTrain, dfValid, targetCol = "Species",
    online = TRUE, returnProbabilityTable = FALSE)

  mmb::setWarnings(w)

  expect_true(is.factor(pred))
  expect_equal(levels(pred), levels(iris$Species))
})


test_that("we get a warning for sparse training data", {
  dfTrain <- iris[1, ]
  dfValid <- iris[150, ]
  expect_true(dfTrain$Species != dfValid$Species)

  w <- mmb::getWarnings()
  mmb::setWarnings(TRUE)

  probTable <- expect_warning({
    mmb::bayesProbabilityAssign(
      dfTrain, dfValid, targetCol = "Species", shiftAmount = 0,
      doEcdf = TRUE, simple = FALSE, returnProbabilityTable = TRUE)
  }, "Training data did not contain all possible labels")

  mmb::setWarnings(w)
})



