#' @title Provides a caret-compatible wrapper around functionality for classification
#' and regression, as implemented by mmb.
#'
#' @description A wrapper to be used with the package/function \code{caret::train()}.
#' Supports regression and classification and an extensive default grid.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @examples \donttest{
#' trainIndex <- caret::createDataPartition(
#'   iris$Species, p = .8, list = FALSE, times = 1)
#' train <- iris[ trainIndex, ]
#' test  <- iris[-trainIndex, ]
#'
#' fitControl <- caret::trainControl(
#'   method = "repeatedcv", number = 2, repeats = 2)
#'
#' fit <- caret::train(
#'   Species ~ ., data = train, method = mmb::bayesCaret,
#'   trControl = fitControl)
#' }
#' @export
bayesCaret <- list(
  # Basics
  type = c("Classification", "Regression"),
  library = c(),

  # Params
  parameters = data.frame(
    parameter = c(
      "shiftAmount",
      "retainMinValues",
      "doEcdf",
      "online",
      "mode",
      "numBuckets",
      "sampleFromAllBuckets",
      "regressor"
    ),
    class = c(
      "numeric",
      "integer",
      "boolean",
      "integer", # Online is an integer with default 0
      "character",
      "integer",
      "boolean",
      "function"
    ),
    label = c(
      "shiftAmount",
      "retainMinValues",
      "doEcdf",
      "online",
      "mode",
      "numBuckets",
      "sampleFromAllBuckets",
      "regressor"
    )
  ),

  prob = NULL
)


# Grid:
bayesCaret$grid <- function(x = NULL, y = NULL, len = NULL, search = "grid") {
  if (missing(y)) {
    warning("y is missing; assuming you want to do regression then?")
  }
  isClassification <- (is.character(y) || is.factor(y))
  numTrain <- NULL

  cOnline <- c(0)
  if (!missing(x)) {
    numTrain <- if (is.data.frame(x)) nrow(x) else length(x)
    cOnline <- c(cOnline, round(1.2 * numTrain))
  } else if (!missing(y)) {
    numTrain <- if (is.data.frame(y)) nrow(y) else length(y)
    cOnline <- c(cOnline, round(1.2 * numTrain))
  }

  modes <- c("full", "simple")
  if (isClassification) {
    modes <- c(modes, "naive") # Naive is only supported in cls.
  }

  gridList <- list(
    shiftAmount = c(0, 0.1),
    doEcdf = c(TRUE, FALSE),
    online = sort(c(cOnline, .Machine$integer.max)),
    mode = modes,
    regressor = NA # is not a function, so will fall back to default for regression
  )

  # Some parameters that depend on whether we do classification or regression:
  rmv <- NULL
  rmvMin <- if (isClassification) 1 else 2
  if (!is.null(numTrain)) {
    rmv <- c(max(rmvMin, ceiling(numTrain * 0.01)), max(rmvMin, ceiling(numTrain * 0.05)))
  }

  if (isClassification) {
    rmv <- c(rmv, 0, 1, 2, 11)
    # no meaning for cls.:
    gridList[["numBuckets"]] <- NA
    gridList[["sampleFromAllBuckets"]] <- FALSE
  } else {
    # note that values < 2 are not allowed for regression
    rmv <- c(rmv, 2, 11)
    # not required for classification; NA leads to always calculating it
    gridList[["numBuckets"]] <- c(5, 10, NA)
    gridList[["sampleFromAllBuckets"]] <- c(TRUE, FALSE)
  }

  gridList[["retainMinValues"]] <- sort(unique(rmv))

  return(expand.grid(gridList))
}

# Fit: There is no fitting with Bayes.
bayesCaret$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  args <- list(...)
  m <- args[["messages"]]
  w <- args[["warnings"]]
  p <- args[["parallel"]]

  if (!is.null(m)) {
    mmb::setMessages(is.logical(m) && m)
  }
  if (!is.null(w)) {
    mmb::setWarnings(is.logical(w) && w)
  }

  theData <- data.frame(x)
  theData$y <- y
  return(list(
    train = theData,
    labelCol = "y",
    param = param,
    # Only use parallelism if not nested or explicitly requested:
    parallel = if (is.logical(p)) p else !foreach::getDoParRegistered()
  ))
}

bayesCaret$predict <- function(modelFit, newdata, submodels) {
  classify <- modelFit$problemType == "Classification"
  theFunc <- if (classify) mmb::bayesProbabilityAssign else mmb::bayesRegressAssign

  default_classification <- list(
    shiftAmount = 0.1,
    retainMinValues = 1,
    doEcdf = FALSE,
    online = 0,
    simple = FALSE,
    naive = FALSE
  )

  default_regression <- list(
    shiftAmount = 0.1,
    retainMinValues = 2,
    doEcdf = FALSE,
    online = 0,
    simple = FALSE,
    numBuckets = ceiling(log2(nrow(modelFit$train))),
    sampleFromAllBuckets = TRUE,
    regressor = NA
  )

  dfParams <- if (classify) default_classification else default_regression
  dfTune <- modelFit$tuneValue

  # Handle the mode:
  dfTune$simple <- dfTune$mode == "simple"
  dfTune$naive <- dfTune$mode == "naive"
  dfTune$mode <- NULL

  # Now let's override everything if/as specified:
  for (n in names(dfParams)) {
    if (n %in% colnames(dfTune)) {
      dfParams[[n]] <- dfTune[[n]]
    }
  }

  # Let's add the common parameters:
  dfParams[["dfTrain"]] <- modelFit$train
  dfParams[["dfValid"]] <- data.frame(newdata)
  dfParams[["targetCol"]] <- modelFit$labelCol
  dfParams[["useParallel"]] <- modelFit$parallel

  return(do.call(theFunc, dfParams))
}
