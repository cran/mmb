# This is required for the foreach::foreach loops.
utils::globalVariables("fn", package = c("mmb"))


#' @title Naive Bayesian inferencing for determining the probability or
#' relative likelihood of a given value.
#'
#' @description A complementary implementation using methods common in mmb,
#' such as computing factors or segmenting data. Supports Laplacian smoothing
#' and early-stopping segmenting, as well as PDF and CDF and selecting any
#' subset of features for dependency.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @keywords naive classification inferencing
#' @param df data.frame that contains all the feature's data
#' @param features data.frame with bayes-features. One of the features needs
#' to be the label-column.
#' @param targetCol string with the name of the feature that represents the
#' label.
#' @param selectedFeatureNames vector default \code{c()}. Vector of strings
#' that are the names of the features the to-predict label depends on. If an
#' empty vector is given, then all of the features are used (except for the
#' label). The order then depends on the features' order.
#' @param shiftAmount numeric an offset value used to increase any one
#' probability (factor) in the full built equation. In scenarios with many
#' dependencies, it is more likely that a single conditional probability
#' becomes zero, which would result in the entire probability being zero.
#' Since this is often useless, the 'shiftAmount' can be added to each
#' factor, resulting in a non-zero probability that can at least be used
#' to order samples by likelihood. Note that, with a positive 'shiftAmount',
#' the result of this function cannot be said to be a probability any
#' longer, but rather results in a comparable likelihood (a 'probability
#' score').
#' @param retainMinValues integer to require a minimum amount of data points
#' when segmenting the data feature by feature.
#' @param doEcdf default FALSE a boolean to indicate whether to use the
#' empirical CDF to return a probability when inferencing a continuous
#' feature. If false, uses the empirical PDF to return the rel. likelihood.
#' This parameter does not have any effect if all of the variables are
#' discrete or when doing a regression. Otherwise, for each continuous
#' variable, the probability to find a value less then or equal - given
#' the conditions - is returned. Note that the interpretation of probability
#' using the ECDF much deviates and must be used with care, especially
#' since it affects each factor in Bayes equation that is continuous. This
#' is especially true for the case where \code{shiftAmount > 0}.
#' @param useParallel default NULL a boolean to indicate whether to use a
#' previously registered parallel backend. If no explicit value was given,
#' calls \code{foreach::getDoParRegistered()} to check for a parallel
#' backend. When using parallelism, this function calculates each factor
#' in the numerator and denominator of the final equation in parallel.
#' @return numeric probability (inferring discrete labels) or relative
#' likelihood (regression, inferring likelihood of continuous value) or most
#' likely value given the conditional features. If using a positive
#' \code{shiftAmount}, the result is a 'probability score'.
#' @examples
#' feat1 <- mmb::createFeatureForBayes(
#'   name = "Petal.Length", value = mean(iris$Petal.Length))
#' feat2 <- mmb::createFeatureForBayes(
#'   name = "Petal.Width", value = mean(iris$Petal.Width))
#' featT <- mmb::createFeatureForBayes(
#'   name = "Species", iris[1,]$Species, isLabel = TRUE)
#'
#' # Check the probability of Species=setosa, given the other 2 features:
#' mmb::bayesProbabilityNaive(
#'   df = iris, features = rbind(feat1, feat2, featT), targetCol = "Species")
#'
#' # Now check the probability of Species=versicolor:
#' featT$valueChar <- "versicolor"
#' mmb::bayesProbabilityNaive(
#'   df = iris, features = rbind(feat1, feat2, featT), targetCol = "Species")
#' @export
bayesProbabilityNaive <- function(
  df, features, targetCol, selectedFeatureNames = c(),
  shiftAmount = 0.1, retainMinValues = 1, doEcdf = FALSE, useParallel = NULL)
{
  bayesSimpleCheckData(df, features, targetCol)
  # Ensure compatibility
  df <- mmb::bayesConvertData(df)

  # One row in features has 'isLabel' = T
  rowOfLabelFeature <- features[features$name == targetCol, ]
  # Let's filter it out, so that we can handle it separately
  featuresWithoutLabel <- features[!features$isLabel, ]

  if (length(selectedFeatureNames) == 0) {
    if (mmb::getMessages()) message("No explicit feature selection, using all.")
    selectedFeatureNames <- featuresWithoutLabel$name
  }

  # Let's further filter the features to use:
  featuresWithoutLabel <- featuresWithoutLabel[
    featuresWithoutLabel$name %in% selectedFeatureNames, ]

  isParRegistered <- foreach::getDoParRegistered()
  useParallel <- if (is.logical(useParallel)) useParallel && isParRegistered else isParRegistered
  if (mmb::getMessages()) {
    if (useParallel) {
      message("Using registered parallel backend.")
    } else {
      message("Using sequential computation.")
    }
  }

  foreachOpFun <- if (useParallel) foreach::`%dopar%` else foreach::`%do%`

  numeratorFactors <- c(
    mmb::bayesComputeMarginalFactor(
      df = df, feature = rowOfLabelFeature, doEcdf = doEcdf))

  # Now the other conditional probabilities depends only
  # on the label given.
  df <- mmb::conditionalDataMin(
    df = df, features = rowOfLabelFeature,
    selectedFeatureNames = targetCol,
    retainMinValues = retainMinValues)

  numeratorFactors <- c(numeratorFactors, foreachOpFun(foreach::foreach(
    fn = featuresWithoutLabel$name,
    .packages = c("mmb", "utils"),
    .combine = c
  ), {
    feat <- featuresWithoutLabel[featuresWithoutLabel$name == fn, ]
    return(mmb::bayesComputeMarginalFactor(df, feat, doEcdf = doEcdf))
  }))

  return(prod(numeratorFactors + shiftAmount))
}
