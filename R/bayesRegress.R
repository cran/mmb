varDefaultRegressor <- make.varClosure(function(data) mmb::estimatePdf(data)$argmax)


#' @title Set a system-wide default regressor.
#'
#' @description Getting and setting the default regressor affects all functions
#' that have an overridable regressor. If this is not given, the default has
#' defined here will be obtained.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @param func a Function to use a regressor, should accept one argument,
#' which is a vector of numeric, and return one value, the regression.
#' @return void
#' @export
setDefaultRegressor <- function(func) {
  varDefaultRegressor$set(func)
}


#' @title Get the system-wide default regressor.
#'
#' @description Getting and setting the default regressor affects all functions
#' that have an overridable regressor. If this is not given, the default has
#' defined here will be obtained.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @return Function the function used as the regressor. Defaults to
#' \code{function(data) mmb::estimatePdf(data)$argmax}.
#' @export
getDefaultRegressor <- function() varDefaultRegressor$get()



#' @title Perform full-dependency Bayesian regression for a sample.
#'
#' @description This method performs full-dependency regression by
#' discretizing the continuous target variable into ranges (buckets),
#' then finding the most probable ranges. It can either regress on
#' the values in the most likely range or sample from all ranges,
#' according to their likelihood.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @keywords full-dependency regression
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
#' @param numBuckets integer the amount of buckets to for discretization.
#' Buckets are built in an equidistant manner, not as quantiles (i.e., one
#' bucket has likely a different amount of values than another).
#' @param sampleFromAllBuckets default TRUE boolean to indicate how to
#' obtain values for regression from the buckets. If true, than takes
#' values from those buckets with a non-zero probability, and according
#' to their probability. If false, selects all values from the bucket
#' with the highest probability.
#' @param regressor Function that is given the collected values for
#' regression and thus finally used to select a most likely value. Defaults
#' to the built-in estimator for the empirical PDF and returns its argmax.
#' However, any other function can be used, too, such as min, max, median,
#' average etc. You may also use this function to obtain the raw values
#' for further processing.
#' @examples
#' w <- mmb::getWarnings()
#' mmb::setWarnings(FALSE)
#'
#' df <- iris[, ]
#' set.seed(84735)
#' rn <- base::sample(rownames(df), 150)
#' dfTrain <- df[1:120, ]
#' dfValid <- df[121:150, ]
#' tf <- mmb::sampleToBayesFeatures(dfValid[1,], "Sepal.Length")
#' mmb::bayesRegress(dfTrain, tf, "Sepal.Length")
#'
#' mmb::setWarnings(w)
#' @export
bayesRegress <- function(
  df, features, targetCol, selectedFeatureNames = c(),
  shiftAmount = 0.1, retainMinValues = 2, doEcdf = FALSE, useParallel = NULL,
  numBuckets = ceiling(log2(nrow(df))),
  sampleFromAllBuckets = TRUE,
  regressor = NULL)
{
  if (is.na(numBuckets)) {
    numBuckets <- ceiling(log2(nrow(df)))
  }
  if (numBuckets < 2) {
    stop("At least two buckets or more are required.")
  }

  if (length(selectedFeatureNames) == 0) {
    if (mmb::getMessages()) message("No explicit feature selection, using all.")
    selectedFeatureNames <- features$name[!(features$name %in% targetCol)]
  }

  if (missing(regressor) || !is.function(regressor)) {
    regressor <- mmb::getDefaultRegressor()
  }

  # Ensure compatibility
  df <- mmb::bayesConvertData(df)

  # We need to attach a new column to the data, that assigns a
  # bucket to each sample, based on the value of the sample's
  # target column.
  rangesBuckets <- mmb::discretizeVariableToRanges(
    df[[targetCol]], numRanges = numBuckets)
  # This'll be the temporary designated name..
  rangeColName <- "__mmb__range__buckets"
  # Create factors with name 1:n, where n is the amount of buckets
  rangeColFactor <- factor(
    as.character(1:length(rangesBuckets)), as.character(1:length(rangesBuckets)))
  rangeColLevels <- levels(rangeColFactor)

  # Attach column with value to our data!
  df[[rangeColName]] <- factor(
    x = sapply(df[[targetCol]], function(x) {
      return(as.character(mmb::getRangeForDiscretizedValue(rangesBuckets, x)))
    }),
    levels = rangeColLevels
  )

  # The next step is to calculate a probability for each bucket, given
  # the sample values for each feature. First, we need to transform the
  # Bayes-features back to a data.frame.
  targetRow <- mmb::bayesFeaturesToSample(
    df[1, !(colnames(df) %in% rangeColName)], features)

  # The result of this is a table with probabilities or relative likelihoods.
  # In the latter case, each row needs to be normalized. Each column of this
  # table responds to the name of one range, and has a probability for it.
  probTable <- mmb::bayesProbabilityAssign(
    dfTrain = df, dfValid = targetRow, targetCol = rangeColName,
    selectedFeatureNames = selectedFeatureNames, shiftAmount = shiftAmount,
    retainMinValues = retainMinValues, doEcdf = doEcdf,
    simple = FALSE, online = 0, returnProbabilityTable = TRUE,
    useParallel = useParallel)

  # Also, there are 2 modes now:
  # sampleFromAllBuckets = F: Take the bucket with the highest probability,
  # and run the regressor on all of its values for the target feature.
  # sampleFromAllBuckets = T: Collect values by sampling from each bucket,
  # according to its probability. Then pass that to the regressor.
  regVals <- c()
  if (sampleFromAllBuckets) {
    probs <- probTable[1, rangeColLevels]
    # normalize it
    probs <- probs / sum(probs)

    for (rcl in rangeColLevels) {
      useFeats <- rbind(
        mmb::createFeatureForBayes(rangeColName, rcl),
        features[!(features$name %in% targetCol), ])

      data <- mmb::conditionalDataMin(
        df = df, features = useFeats,
        selectedFeatureNames = c(selectedFeatureNames, rangeColName),
        retainMinValues = retainMinValues)

      numSamples <- min(round(probs[[rcl]] * nrow(data)), nrow(data))
      if (numSamples > 0) {
        regVals <- c(regVals, base::sample(data[[targetCol]], numSamples))
      }
    }
  } else {
    rangeName <- names(which.max(probTable[1, ]))[1]
    # Add a new feature with the bucket, to filter further.
    features <- rbind(
      mmb::createFeatureForBayes(rangeColName, rangeName),
      features[!(features$name %in% targetCol), ])

    # Select data matching all the original sample's values and the bucket.
    data <- mmb::conditionalDataMin(
      df = df, features = features,
      selectedFeatureNames = c(selectedFeatureNames, rangeColName),
      retainMinValues = retainMinValues)
    regVals <- data[[targetCol]]
  }

  regVal <- tryCatch({
    val <- regressor(regVals)
    if (is.nan(val) && mmb::getWarnings()) {
      warning("Regressor returned NaN.")
    }
    return(val)
  }, error = function(cond) {
    if (mmb::getWarnings()) {
      warning(paste("Regressor failed:", cond))
    }
    return(NaN)
  })

  return(regVal)
}



#' @title Regression for one or more samples, given some training data.
#'
#' @description This method uses full-dependency (\code{simple=F}) Bayesian
#' inferencing to to a regression for the target features for all of the
#' samples given in \code{dfValid}. Assigns a regression value using either
#' @seealso \code{mmb::bayesRegress()} (full) or @seealso \code{mmb::bayesRegressSimple()}
#' if \code{simple=T}. It mostly forwards the given arguments to these functions,
#' and you will find good documentation there.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @keywords full-dependency regression
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
#' good. If true, uses \code{mmb::bayesRegressSimple()}. Otherwise, uses
#' \code{mmb::bayesRegress()}.
#' @param useParallel boolean DEFAULT NULL this is forwarded to the underlying
#' function \code{mmb::bayesRegress()} (only in simple=FALSE mode).
#' @param numBuckets integer the amount of buckets to for discretization.
#' Buckets are built in an equidistant manner, not as quantiles (i.e., one
#' bucket has likely a different amount of values than another).
#' @param sampleFromAllBuckets default TRUE boolean to indicate how to
#' obtain values for regression from the buckets. If true, than takes
#' values from those buckets with a non-zero probability, and according
#' to their probability. If false, selects all values from the bucket
#' with the highest probability.
#' @param regressor Function that is given the collected values for
#' regression and thus finally used to select a most likely value. Defaults
#' to the built-in estimator for the empirical PDF and returns its argmax.
#' However, any other function can be used, too, such as min, max, median,
#' average etc. You may also use this function to obtain the raw values
#' for further processing.#'
#' @examples \donttest{
#' df <- iris[, ]
#' set.seed(84735)
#' rn <- base::sample(rownames(df), 150)
#' dfTrain <- df[1:120, ]
#' dfValid <- df[121:150, ]
#' res <- mmb::bayesRegressAssign(
#'   dfTrain, dfValid[, !(colnames(dfValid) %in% "Sepal.Length")],
#'   "Sepal.Length", sampleFromAllBuckets = TRUE, doEcdf = TRUE)
#' cov(res, iris[121:150,]$Sepal.Length)^2
#' }
#' @export
bayesRegressAssign <- function(
  dfTrain, dfValid, targetCol, selectedFeatureNames = c(),
  shiftAmount = 0.1, retainMinValues = 2, doEcdf = FALSE,
  online = 0,
  simple = FALSE,
  useParallel = NULL,
  numBuckets = ceiling(log2(nrow(df))),
  sampleFromAllBuckets = TRUE,
  regressor = NULL)
{
  # Ensure compat.
  df <- mmb::bayesConvertData(dfTrain)
  dfValid <- mmb::bayesConvertData(dfValid)

  if (is.na(numBuckets)) {
    numBuckets <- ceiling(log2(nrow(df)))
  }

  if (!(targetCol %in% colnames(dfValid))) {
    # Needs to be present for sampleToBayesFeatures(..)
    dfValid[[targetCol]] <- 0
  }

  if (missing(regressor) || !is.function(regressor)) {
    regressor <- mmb::getDefaultRegressor()
  }

  predicted <- c()

  for (n in rownames(dfValid)) {
    tf <- mmb::sampleToBayesFeatures(dfValid[n, ], targetCol)

    pred <- 0
    if (simple) {
      pred <- mmb::bayesRegressSimple(
        df = df, features = tf, targetCol = targetCol,
        selectedFeatureNames = selectedFeatureNames,
        retainMinValues = retainMinValues)
    } else {
      pred <- mmb::bayesRegress(
        df = df, features = tf, targetCol = targetCol,
        selectedFeatureNames = selectedFeatureNames,
        shiftAmount = shiftAmount, retainMinValues = retainMinValues,
        doEcdf = doEcdf, useParallel = useParallel, numBuckets = numBuckets,
        sampleFromAllBuckets = sampleFromAllBuckets,
        regressor = regressor)
    }

    predicted <- c(predicted, pred)

    if (online > 0 && !is.nan(pred)) {
      tempRow <- dfValid[n, ]
      tempRow[[targetCol]] <- pred
      df <- rbind(df, tempRow)
      df <- utils::tail(df, online)
    }
  }

  return(predicted)
}

