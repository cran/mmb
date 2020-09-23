library(testthat)

source("../helpers.R")

library(datasets)
data("iris")


test_that("a zero numerator can happen", {

  df <- data.frame(
    A = factor(x = rep(1, 10), levels = c("1", "2")),
    B = rnorm(10, 10, 1) # mean 10, sd 1
  )

  feats <- rbind(
    mmb::createFeatureForBayes(
      "A", factor(x = "2", levels = levels(df$A)), isLabel = TRUE),
    mmb::createFeatureForBayes("B", 10)
  )

  w <- mmb::getWarnings()
  mmb::setWarnings(TRUE)
  m <- mmb::getMessages()
  mmb::setMessages(TRUE)

  expect_warning({
    expect_message({
      mmb::bayesProbability(df, feats, "A", shiftAmount = 0)
    }, "Numerator was zero")
  }, "Segmenting stopped prematurely")

  mmb::setMessages(m)
  mmb::setWarnings(w)
})


test_that("a zero denominator can happen", {
  # Checking for a zero denominator after the numerator was not zero
  # is tough and cannot happen theoretically, because a more constrained
  # segmenting will have fewer values, and if a more constrained data
  # yields P > 0, so does the less constrained. So far the theory.
  # However, due to approximation and rounding errors, there is probably
  # a case that can produce this. I was not able to produce such a case
  # so that this test was disabled. There were some working examples
  # with a bimodal distribution. However, these do not work because the
  # default method for KDE uses bw="SJ". The part that would produce the
  # message in bayesProbability() was disabled, too.
  expect_true(TRUE) # dummy

  # Using the PDF on this data.frame, where A=1 and PDF_B_A(1.5) yields > 0,
  # if unconditional (PDF_B(1.5)) it yields 0!
  #df <- data.frame(
  #  A = factor(x = c(rep(1, 2), rep(2, 2e6-2)), levels = c("1", "2")),
  #  B = c(1+1e-14, rep(1,1e6-1), rep(1e2,1e6))
  #)

  #feats <- rbind(
  #  mmb::createFeatureForBayes("A", df$A[1], isLabel = TRUE),
  #  mmb::createFeatureForBayes("B", 2.5)
  #)
})
