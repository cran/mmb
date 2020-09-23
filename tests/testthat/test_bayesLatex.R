library(testthat)

source("../helpers.R")

library(datasets)
data("iris")


test_that("conversion to latex-equations work", {
  df <- data.frame(
    A = iris$Species,
    B = iris$Sepal.Length,
    C = iris$Petal.Width
  )

  cf <- rbind(
    mmb::createFeatureForBayes("B", round(mean(df$B), 2)),
    mmb::createFeatureForBayes("C", round(mean(df$C), 2))
  )

  tf <- mmb::createFeatureForBayes("A", df$A[1], isLabel = TRUE)

  w <- mmb::getWarnings()
  mmb::setWarnings(TRUE)

  expect_warning({
    mmb::bayesToLatex(tf, cf)
  })

  expect_does_throw({
    mmb::bayesToLatex(data.frame(), tf)
  })
  expect_does_throw({
    mmb::bayesToLatex(cf, data.frame())
  })


  expect_equal(mmb::bayesToLatex(cf, tf, F), "P(\\textit{A} | \\textit{B},\\textit{C}) = \\frac{P(\\textit{B} | \\textit{C},\\textit{A}) \\times P(\\textit{C} | \\textit{A}) \\times P(\\textit{A})}{P(\\textit{B} | \\textit{C}) \\times P(\\textit{C})}")
  expect_equal(mmb::bayesToLatex(cf, tf, T), "P(\\textit{A} | \\textit{B},\\textit{C}) = \\frac{P(\\textit{B} \\leq 5.84 | \\textit{C} \\leq 1.2,\\textit{A} = \\text{setosa}) \\times P(\\textit{C} \\leq 1.2 | \\textit{A} = \\text{setosa}) \\times P(\\textit{A} = \\text{setosa})}{P(\\textit{B} \\leq 5.84 | \\textit{C} \\leq 1.2) \\times P(\\textit{C} \\leq 1.2)}")

  mmb::setWarnings(w)
})
