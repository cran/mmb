# This is required for the foreach::foreach loops.
utils::globalVariables("featIdx", package = c("mmb"))


#' @title Computes one single factor that is needed for full Bayesian inferencing.
#'
#' @description In an equation such as P(A|B) = P(B|A) * P(A) / P(B), the target
#' feature is A, while the conditional feature is B. There are three factors in that
#' equation (two in the numerator and one in the denominator). This function
#' calculates exactly one factor and expects all features to be given in the
#' right order. If computing the denominator, no target-feature is required.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{bayes1763lii}{mmb}
#' @param df data.frame with data that is used to segment
#' @param conditionalFeatures data.frame with Bayesian features, as produced
#' by @seealso \code{mmb::createFeatureForBayes()}. This data.frame must not
#' be empty, as we need to depend on at least one feature.
#' @param targetFeature data.frame with exactly one Bayesian feature. Any
#' excessive features are discarded and a warning is produced. If computing a
#' factor for the denominator, this data.frame may be empty.
#' @param computeNumerator boolean to indicate whether a factor for the
#' numerator is build. In that case, the target feature is required.
#' @param retainMinValues integer the amount of rows to minimally retain
#' during segmenting using the conditional features.
#' @param doEcdf default FALSE a boolean to indicate whether to use the
#' empirical CDF to return a probability when inferencing a continuous
#' feature. If false, uses the empirical PDF to return the rel. likelihood.
#' This parameter does not have any effect when inferring discrete values.
#' Using the ECDF, a probability to find a value less than or equal to the
#' given value is returned. Setting this parameter to true in conjunction
#' with a non-zero shiftAmount must be done with caution.
#' @return numeric the factor as probability or relative likelihood. If the
#' target feature is discrete, a probability is returned; a relative
#' likelihood, otherwise.
#' @keywords internal
bayesComputeProductFactor <- function(
  df, conditionalFeatures, targetFeature,
  computeNumerator, retainMinValues = 1, doEcdf = FALSE)
{
  if (nrow(conditionalFeatures) == 0) stop("At least one conditional feature is required.")
  if (nrow(targetFeature) > 1 && mmb::getWarnings()) {
    warning("More than one target feature given, taking first, ignoring rest.")
  }

  # Ensure compat.
  df <- mmb::bayesConvertData(df)

  probFeat <- utils::head(conditionalFeatures, 1) # Always the first conditional feature
  features <- NULL
  if (computeNumerator) {
    if (nrow(targetFeature) == 0) stop("The numerator requires the target-feature.")
    features <- rbind(utils::tail(conditionalFeatures, -1), targetFeature)
  } else {
    features <- utils::tail(conditionalFeatures, -1)
  }

  # In the case we are given a single marginal feature that is
  # not the target feature, segmenting is not required.
  data <- if (nrow(features) == 0) df else mmb::conditionalDataMin(
    df, features, selectedFeatureNames = features$name, retainMinValues)
  fac <- NULL
  if (probFeat$isDiscrete) {
    if (mmb::getMessages()) message(
      paste("Getting conditional probability for", probFeat$name))

    fac <- mmb::getProbForDiscrete(
      data[[probFeat$name]], mmb::getValueOfBayesFeatures(probFeat, probFeat$name))
  } else {
    featVal <- mmb::getValueOfBayesFeatures(probFeat, probFeat$name)
    dataFeature <- data[[probFeat$name]]

    if (doEcdf) {
      if (length(dataFeature) == 0 && mmb::getWarnings()) {
        warning("No data left for ECDF after segmenting.")
        fac <- 0
      } else {
        fac <- stats::ecdf(dataFeature)(featVal)
      }
    } else {
      if (mmb::getMessages()) message(
        paste("Building PDF(", probFeat$name,") given ",
              paste(features$name, collapse = ","), sep = ""))

      dataPdf <- mmb::estimatePdf(dataFeature)
      fac <- dataPdf$fun(featVal)
    }
  }

  return(fac)
}


#' @title Compute a marginal factor (continuous or discrete random variable).
#'
#' @description Computes the probability (discrete feature) or relative likelihood
#' (continuous feature) of one given feature and a concrete value for it.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @param df data.frame that contains all the feature's data
#' @param feature data.frame containing the designated feature as created
#' by @seealso \code{mmb::createFeatureForBayes()}.
#' @param doEcdf default FALSE a boolean to indicate whether to use the
#' empirical CDF to return a probability when inferencing a continuous
#' feature. If false, uses the empirical PDF to return the rel. likelihood.
#' This parameter does not have any effect when inferring discrete values.
#' Using the ECDF, a probability to find a value less than or equal to the
#' given value is returned.
#' @return numeric the probability or likelihood of the given feature
#' assuming its given value.
#' @examples
#' feat <- mmb::createFeatureForBayes(
#'   name = "Petal.Length", value = mean(iris$Petal.Length))
#' mmb::bayesComputeMarginalFactor(df = iris, feature = feat)
#' mmb::bayesComputeMarginalFactor(df = iris, feature = feat, doEcdf = TRUE)
#' @export
bayesComputeMarginalFactor <- function(df, feature, doEcdf = FALSE) {
  prob <- 0
  if (feature$isDiscrete) {
    if (doEcdf && mmb::getWarnings()) {
      warning("Requesting ECDF, but has no effect for discrete variables.")
    }
    prob <- mmb::getProbForDiscrete(
      df[[feature$name]],
      mmb::getValueOfBayesFeatures(feature, feature$name))
  } else {
    featVal <- mmb::getValueOfBayesFeatures(feature, feature$name)
    if (doEcdf) {
      if (nrow(df) == 0) {
        if (mmb::getWarnings()) warning("No data given for estimating the CDF.")
        prob <- 0
      } else {
        prob <- stats::ecdf(df[[feature$name]])(featVal)
      }
    } else {
      pdf <- mmb::estimatePdf(df[[feature$name]])
      prob <- pdf$fun(mmb::getValueOfBayesFeatures(feature, feature$name))
    }
  }
  return(prob)
}



#' @title Full Bayesian inferencing for determining the probability or
#' relative likelihood of a given value.
#'
#' @description
#' Uses the full extended theorem of Bayes, taking all selected features
#' into account. Expands Bayes' theorem to accomodate all dependent
#' features, then calculates each conditional probability (or relative
#' likelihood) and returns a single result reflecting the probability or
#' relative likelihood of the target feature assuming its given value,
#' given that all the other dependent features assume their given value.
#' The target feature (designated by 'labelCol') may be discrete or continuous.
#' If at least one of the depending features or the the target feature
#' is continuous and the PDF ('doEcdf' = FALSE) is built, the result of this
#' function is a relative likelihood of the target feature's value. If
#' all of the features are discrete or the empirical CDF is used instead
#' of the PDF, the result of this function is a probability.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @keywords full-dependency classification inferencing
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{bayes1763lii}{mmb}
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
#' mmb::bayesProbability(
#'   df = iris, features = rbind(feat1, feat2, featT), targetCol = "Species")
#'
#' # Now check the probability of Species=versicolor:
#' featT$valueChar <- "versicolor"
#' mmb::bayesProbability(
#'   df = iris, features = rbind(feat1, feat2, featT), targetCol = "Species")
#' @export
bayesProbability <- function(
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
  # Now order:
  featuresWithoutLabel <- featuresWithoutLabel[
    match(selectedFeatureNames, featuresWithoutLabel$name), ]

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
  computeFac <- function(compNum, featIdx, targetFeat = data.frame()) {
    # Here, we need to slice the features without labels,
    # beginning from the current featIdx.
    condFeats <- featuresWithoutLabel[
      rownames(featuresWithoutLabel) >= featIdx, ]

    return(bayesComputeProductFactor(
      df, condFeats, targetFeat, computeNumerator = compNum,
      retainMinValues = retainMinValues, doEcdf = doEcdf))
  }

  numeratorFactors <- c()
  numeratorFactors <- foreachOpFun(foreach::foreach(
    featIdx = rownames(featuresWithoutLabel),
    .packages = c("mmb", "utils"),
    .combine = c
  ), {
    return(computeFac(TRUE, featIdx, rowOfLabelFeature))
  })

  # Add the marginal:
  numeratorFactors <- c(
    numeratorFactors,
    mmb::bayesComputeMarginalFactor(
      df, rowOfLabelFeature, doEcdf = doEcdf && !rowOfLabelFeature$isDiscrete))

  prodNumerator <- prod(numeratorFactors + shiftAmount)
  if (prodNumerator == 0) {
    if (mmb::getMessages()) message("Numerator was zero, skipping denominator.")
    return(0)
  }

  # Now the denominator:
  denominatorFactors <- c()
  denominatorFactors <- foreachOpFun(foreach::foreach(
    featIdx = rownames(featuresWithoutLabel),
    .packages = c("mmb", "utils"),
    .combine = c
  ), {
    # Here, we do almost the same, except for that we
    # do not require the target feature.
    return(computeFac(FALSE, featIdx))
  })


  prodDenominator <- prod(denominatorFactors + shiftAmount)
  # The following was disabled as it cannot happen in theory and because
  # I was not able to produce a synthetic test that could provoke this.
  #' @seealso test-case "a zero denominator can happen"
  # If you manage to produce such a case please contact me.
  #
  #if (prodDenominator == 0) {
  #  if (mmb::getMessages()) message("Denominator was zero, probability is zero.")
  #  return(0)
  #}

  # OK, both numerator and denominator were > 0!
  return(prodNumerator / prodDenominator)
}


#' @title Assign probabilities to one or more samples, given some training data.
#'
#' @description This method uses full-dependency (\code{simple=F}) Bayesian
#' inferencing to assign a probability to the target feature in all of the
#' samples given in \code{dfValid}. Tests each sample using @seealso
#' \code{mmb::bayesProbability()} or @seealso \code{mmb::bayesProbabilitySimple()}.
#' It mostly forwards the given arguments to these functions, and you will find
#' good documentation there.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @keywords full-dependency classification inferencing
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{bayes1763lii}{mmb}
#' @param dfTrain data.frame that holds the training data.
#' @param dfValid data.frame that holds the validation samples, for each of which
#' a probability is sought. The convention is, that if you attempt to assign a
#' probability to a numeric value, it ought to be found in the target column of
#' this data frame (otherwise, the target column is not required in it).
#' @param targetCol character the name of targeted feature, i.e., the feature to
#' assign a probability to.
#' @param selectedFeatureNames character defaults to empty vector which defaults
#' to using all available features. Use this to select subsets of features and to
#' order features.
#' @param shiftAmount numeric an offset value used to increase any one
#' probability (factor) in the full built equation.
#' @param retainMinValues integer to require a minimum amount of data points
#' when segmenting the data feature by feature.
#' @param doEcdf default FALSE a boolean to indicate whether to use the
#' empirical CDF to return a probability when inferencing a continuous
#' feature.
#' @param online default 0 integer to indicate how many rows should be used to
#' do inferencing. If zero, then only the initially given data.frame dfTrain is
#' used. If > 0, then each inferenced sample will be attached to it and the
#' resulting data.frame is truncated to this number. Use an integer large enough
#' (i.e., sum of training and validation rows) to keep all samples during
#' inferencing. A smaller amount as, e.g., in dfTrain, will keep the amount of data
#' restricted, discarding older rows. A larger amount than, e.g., in dfTrain is
#' also fine; dfTrain will grow to it and then discard rows.
#' @param simple default FALSE boolean to indicate whether or not to use simple
#' Bayesian inferencing instead of full. This is faster but the results are less
#' good. If true, uses \code{mmb::bayesProbabilitySimple()}. Otherwise, uses
#' \code{mmb::bayesProbability()}.
#' @param naive default FALSE boolean to indicate whether or not to use naive
#' Bayesian inferencing instead of full or simple.
#' @param useParallel boolean DEFAULT NULL this is forwarded to the underlying
#' function \code{mmb::bayesProbability()} (only in simple=FALSE mode).
#' @param returnProbabilityTable default FALSE boolean to indicate whether to
#' return only the probabilities for each validation sample or whether a table
#' with a probability for each tested label should be returned. This has no
#' effect when inferencing probabilities for numeric values, as the table then
#' only has one column "probability". The first column of this table is always
#' called "rowname" and corresponds to the rownames of dfValid.
#' @examples
#' w <- mmb::getWarnings()
#' mmb::setWarnings(FALSE)
#'
#' set.seed(84735)
#' rn <- base::sample(rownames(iris), 150)
#' dfTrain <- iris[rn[1:120], ]
#' dfValid <- iris[rn[121:150], !(colnames(iris) %in% "Species") ]
#' mmb::bayesProbabilityAssign(dfTrain, dfValid, "Species")
#'
#' mmb::setWarnings(w)
#' @export
bayesProbabilityAssign <- function(
  dfTrain, dfValid, targetCol, selectedFeatureNames = c(),
  shiftAmount = 0.1, retainMinValues = 1, doEcdf = FALSE,
  online = 0,
  simple = FALSE,
  naive = FALSE,
  useParallel = NULL,
  returnProbabilityTable = FALSE)
{
  # Either predict a discrete label -OR- a probability for a numeric value.
  # In the latter case, dfValid needs to present a value in targetCol.
  predictNumProb <- is.numeric(dfTrain[[targetCol]])
  if (predictNumProb && !(targetCol %in% colnames(dfValid))) {
    stop("Requested to predict a probability for numeric value, but not present in validation data.")
  }

  if (simple && naive) {
    stop("Can only either do full, simple or naive.")
  }

  targetIsFactor <- is.factor(dfTrain[[targetCol]])
  lvls <- if (targetIsFactor) levels(dfTrain[[targetCol]]) else unique(c(dfTrain[[targetCol]]))
  # Ensure compat.
  df <- mmb::bayesConvertData(dfTrain)
  dfValid <- mmb::bayesConvertData(dfValid)

  predicted <- c()
  # We only test labels found in the training data.
  targetLabels <- unique(c(df[[targetCol]]))
  probTableCols <- c("rowname", if (predictNumProb) "probability" else targetLabels)
  probTable <- data.frame(matrix(nrow = 0, ncol = length(probTableCols)))
  colnames(probTable) <- c("rowname", if (predictNumProb) "probability" else targetLabels)
  for (n in rownames(dfValid)) {
    probRow <- data.frame(matrix(nrow = 1, ncol = length(probTableCols)))
    colnames(probRow) <- colnames(probTable)
    probRow$rowname <- n

    # The values we will test for each validation row
    testValues <- if (predictNumProb) dfValid[n, targetCol] else targetLabels
    for (tv in testValues) {
      dfValid[n, targetCol] <- tv
      sample <- mmb::sampleToBayesFeatures(dfValid[n, ], targetCol)

      predProb <- 0
      if (simple) {
        predProb <- mmb::bayesProbabilitySimple(
          df = df, features = sample, targetCol = targetCol,
          selectedFeatureNames = selectedFeatureNames,
          retainMinValues = retainMinValues, doEcdf = doEcdf)
      } else if (naive) {
        predProb <- mmb::bayesProbabilityNaive(
          df = df, features = sample, targetCol = targetCol,
          selectedFeatureNames = selectedFeatureNames,
          shiftAmount = shiftAmount, retainMinValues = retainMinValues,
          doEcdf = doEcdf, useParallel = useParallel)
      } else {
        predProb <- mmb::bayesProbability(
          df = df, features = sample, targetCol = targetCol,
          selectedFeatureNames = selectedFeatureNames,
          shiftAmount = shiftAmount, retainMinValues = retainMinValues,
          doEcdf = doEcdf, useParallel = useParallel)
      }

      probRow[1, if (predictNumProb) "probability" else tv] <- predProb
    }

    probTable <- rbind(probTable, probRow)

    # If this is supposed to run online, then append to the
    # training data what was just predicted.
    if (online > 0) {
      res <- if (predictNumProb) probRow[1, "probability"] else names(which.max(probRow[1, targetLabels]))[1]
      tempRow <- dfValid[n, ]
      tempRow[[targetCol]] <- res
      df <- rbind(df, tempRow)
      df <- utils::tail(df, online)
    }
  }

  # Check if found target-labels were the same as the levels
  # and fill probTable with zeros for those not computed.
  if (targetIsFactor && length(lvls) > length(targetLabels)) {
    notTested <- setdiff(lvls, targetLabels)
    if (mmb::getWarnings()) warning(paste(
      "Training data did not contain all possible labels. Setting labels",
      paste(notTested, collapse = ", "),"to 0."))

    for (fac in notTested) {
      probTable[[fac]] <- 0
    }
  }

  if (returnProbabilityTable) {
    return(probTable)
  }

  # .. otherwise, return only the blank predictions:
  if (predictNumProb) {
    return(probTable[["probability"]])
  }
  # .. Ok, return only predictions of discrete label,
  # and only those with highest probability:
  predicted <- c()
  for (rn in rownames(probTable)) {
    predicted <- c(predicted, names(which.max(probTable[rn, targetLabels]))[1])
  }

  # Transform back to factor if needed:
  if (targetIsFactor) {
    predicted <- factor(predicted, levels = lvls)
  }

  return(predicted)
}

