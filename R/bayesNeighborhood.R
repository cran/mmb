# This is required for the foreach::foreach loops.
utils::globalVariables("rn", package = c("mmb"))

#' @title Given Bayesian features, returns those samples from a dataset that
#' exhibit a similarity (i.e., the neighborhood).
#'
#' @description The neighborhood \eqn{N_i} is defined as the set of samples that
#' have a similarity greater than zero to the given sample \eqn{s_i}. Segmentation
#' is done using equality (\code{==}) for discrete features and less than or equal
#' (\code{<=}) for continuous features. Note that feature values \code{NA} and \code{NaN}
#' are also supported using \code{is.na()} and \code{is.nan()}.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @keywords network
#' @param df data.frame to select the neighborhood from
#' @param features data.frame of Bayes-features, used to segment/select the
#' rows that should make up the neighborhood.
#' @param selectedFeatureNames vector of names of features to use to demarcate
#' the neighborhood. If empty, uses all features' names.
#' @param retainMinValues DEFAULT 0 the amount of samples to retain during
#' segmentation. For separating a neighborhood, this value typically should
#' be 0, so that no samples are included that are not within it. However,
#' for very sparse data or a great amount of variables, it might still make
#' sense to retain samples.
#' @return data.frame with rows that were selected as neighborhood. It is
#' guaranteed that the rownames are maintained.
#' @examples
#' nbh <- mmb::neighborhood(df = iris, features = mmb::createFeatureForBayes(
#'   name = "Sepal.Width", value = mean(iris$Sepal.Width)))
#'
#' print(nrow(nbh))
#' @export
neighborhood <- function(df, features, selectedFeatureNames = c(), retainMinValues = 0) {
  if (length(selectedFeatureNames) == 0) {
    if (mmb::getWarnings()) warning("No explicit feature selection, using all.")
    selectedFeatureNames <- features$name
  }

  return(mmb::conditionalDataMin(
    df = df, features = features,
    selectedFeatureNames = selectedFeatureNames,
    retainMinValues = retainMinValues))
}


#' @title Given a neighborhood of data, computes the similarity of each sample
#' in the neighborhood to the neighborhood.
#'
#' @description Takes a data.frame of samples, then builds a PDF/PMF or ECDF
#' for each of the selected features. Then, for each sample, computes the product
#' of probabilities. The result is a vector that holds a probability for each
#' sample. That probability (or relative likelihood) then represents the
#' vicinity (or similarity) of the sample to the given neighborhood.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @keywords network
#' @param dfNeighborhood data.frame that holds all rows that make up the neighborhood.
#' @param selectedFeatureNames vector of names of features to use. The centrality
#' of each row in the neighborhood is calculated based on the selected features.
#' @param shiftAmount numeric DEFAULT 0.1 optional amount to shift each features
#' probability by. This is useful for when the centrality not necessarily must be
#' an actual probability and too many features are selected. To obtain actual
#' probabilities, this needs to be 0, and you must use the ECDF.
#' @param doEcdf boolean DEFAULT FALSE whether to use the ECDF instead of the EPDF
#' to find the likelihood of continuous values.
#' @param ecdfMinusOne boolean DEFAULT FALSE only has an effect if the ECDF is
#' used. If true, uses 1 minus the ECDF to find the probability of a continuous
#' value. Depending on the interpretation of what you try to do, this may be of use.
#' @return a named vector, where the names correspond to the rownames of the rows
#' in the given neighborhood, and the value is the centrality of that row.
#' @examples
#' # Create a neighborhood:
#' nbh <- mmb::neighborhood(df = iris, features = mmb::createFeatureForBayes(
#'   name = "Sepal.Width", value = mean(iris$Sepal.Width)))
#'
#' cent <- mmb::centralities(dfNeighborhood = nbh, shiftAmount = 0.1,
#'   doEcdf = TRUE, ecdfMinusOne = TRUE)
#'
#' # Plot the ordered samples to get an idea of the centralities in the neighborhood:
#' plot(x = names(cent), y=cent)
#' @export
centralities <- function(
  dfNeighborhood, selectedFeatureNames = c(),
  shiftAmount = 0.1, doEcdf = FALSE, ecdfMinusOne = FALSE)
{

  if (!is.data.frame(dfNeighborhood)) {
    stop("dfNeighborhood is not a data.frame.")
  }

  if (length(selectedFeatureNames) == 0) {
    if (mmb::getWarnings()) warning("No explicit feature selection, using all.")
    selectedFeatureNames <- colnames(dfNeighborhood)
  }

  if (nrow(dfNeighborhood) == 0) {
    if (mmb::getWarnings()) warning("dfNeighborhood is empty.")
    return(c())
  }

  if (ecdfMinusOne && !doEcdf && mmb::getWarnings()) {
    warning("ecdfMinusOne=TRUE without doEcdf=TRUE has no effect.")
  }

  df <- mmb::bayesConvertData(dfNeighborhood)

  # Store the estimators
  estFuncs <- list()
  for (sfn in selectedFeatureNames) {
    # We use this as a helper to check if a feature is discrete.
    feat <- mmb::createFeatureForBayes(sfn, df[[1, sfn]])

    if (feat$isDiscrete) { # PMF
      estFuncs[[sfn]] <- (function() {
        distr <- table(df[[sfn]])
        return(function(x) distr[[x]] / sum(distr))
      })()
    } else {
      if (doEcdf) {
        estFuncs[[sfn]] <- (function() {
          cdf <- stats::ecdf(df[[sfn]])
          return(function(x) if (ecdfMinusOne) 1 - cdf(x) else cdf(x))
        })()
      } else {
        estFuncs[[sfn]] <- (function() {
          pdf <- mmb::estimatePdf(df[[sfn]])
          return(function(x) pdf$fun(x))
        })()
      }
    }
  }


  centralities <- c()
  for (rn in rownames(df)) {
    sample <- df[rn, ]
    probs <- c()

    for (featName in names(estFuncs)) {
      probs <- c(probs, estFuncs[[featName]](sample[[featName]]))
    }

    centralities[[rn]] <- prod(probs + shiftAmount)
  }

  return(centralities)
}


#' @title Given a neighborhood of data and two samples from that neighborhood,
#' calculates the distance between the samples.
#'
#' @description The distance of two samples x,y from each other within a given
#' neighborhood is defined as the absolute value of the subtraction of each
#' sample's centrality to the neighborhood.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @keywords network
#' @param dfNeighborhood data.frame that holds all rows that make up the neighborhood.
#' @param rowNrOfSample1 character the name of the row that constitutes the first
#' sample from the given neighborhood.
#' @param rowNrOfSample2 character the name of the row that constitutes the second
#' sample from the given neighborhood.
#' @param selectedFeatureNames vector of names of features to use. The centrality
#' of each row in the neighborhood is calculated based on the selected features.
#' @param shiftAmount numeric DEFAULT 0.1 optional amount to shift each features
#' probability by. This is useful for when the centrality not necessarily must be
#' an actual probability and too many features are selected. To obtain actual
#' probabilities, this needs to be 0, and you must use the ECDF.
#' @param doEcdf boolean DEFAULT FALSE whether to use the ECDF instead of the EPDF
#' to find the likelihood of continuous values.
#' @param ecdfMinusOne boolean DEFAULT FALSE only has an effect if the ECDF is
#' used. If true, uses 1 minus the ECDF to find the probability of a continuous
#' value. Depending on the interpretation of what you try to do, this may be of use.
#' @return numeric the distance as a positive number.
#' @examples
#' # Show the distance between two samples using all their features:
#' mmb::distance(dfNeighborhood = iris, rowNrOfSample1 = 10, rowNrOfSample2 = 99)
#'
#' # Let's use an actual neighborhood:
#' nbh <- mmb::neighborhood(df = iris, features = mmb::createFeatureForBayes(
#'   name = "Sepal.Length", value = mean(iris$Sepal.Length)))
#' mmb::distance(dfNeighborhood = nbh, rowNrOfSample1 = 1, rowNrOfSample2 = 30,
#'   selectedFeatureNames = colnames(iris)[1:3])
#'
#' # Let's compare this to the distances as they are in iris (should be smaller):
#' mmb::distance(dfNeighborhood = iris, rowNrOfSample1 = 1, rowNrOfSample2 = 30,
#'   selectedFeatureNames = colnames(iris)[1:3])
#' @export
distance <- function(
  dfNeighborhood, rowNrOfSample1, rowNrOfSample2, selectedFeatureNames = c(),
  shiftAmount = 0.1, doEcdf = FALSE, ecdfMinusOne = FALSE
) {
  cent <- mmb::centralities(
    dfNeighborhood = dfNeighborhood, selectedFeatureNames = selectedFeatureNames,
    shiftAmount = shiftAmount, doEcdf = doEcdf, ecdfMinusOne = ecdfMinusOne
  )
  return(abs(cent[[rowNrOfSample1]] - cent[[rowNrOfSample2]]))
}


#' @title Segment a dataset by a single sample and compute vicinities for it and
#' the remaining samples in the neighborhood.
#'
#' @description Given some data and one sample \eqn{s_i} from it, constructs the
#' neighborhood \eqn{N_i} of that sample and assigns centralities to all other
#' samples in that neighborhood to it. Samples that lie outside the neighborhood
#' are assigned a vicinity of zero. Uses \code{mmb::neighborhood()} and
#' \code{mmb::centralities()}.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @keywords network
#' @param df data.frame that holds the data (and also the sample to use to define
#' the neighborhood). Each sample in this data.frame is assigned a vicinity.
#' @param sampleFromDf data.frame a single row from the given data.frame. This is
#' used to select a neighborhood from the given data.
#' @param selectedFeatureNames vector of names of features to use to compute the
#' vicinity/centrality. This is passed to \code{mmb::neighborhood()}.
#' @param shiftAmount numeric DEFAULT 0.1 optional amount to shift each features
#' probability by. This is useful for when the centrality not necessarily must be
#' an actual probability and too many features are selected. To obtain actual
#' probabilities, this needs to be 0, and you must use the ECDF.
#' @param doEcdf boolean DEFAULT FALSE whether to use the ECDF instead of the EPDF
#' to find the likelihood of continuous values.
#' @param ecdfMinusOne boolean DEFAULT FALSE only has an effect if the ECDF is
#' used. If true, uses 1 minus the ECDF to find the probability of a continuous
#' value. Depending on the interpretation of what you try to do, this may be of use.
#' @param retainMinValues DEFAULT 0 the amount of samples to retain during
#' segmentation. For separating a neighborhood, this value typically should
#' be 0, so that no samples are included that are not within it. However,
#' for very sparse data or a great amount of variables, it might still make
#' sense to retain samples.
#' @return data.frame with a single column 'vicinity' and the same rownames as the
#' given data.frame. Each row then holds the vicinity for the corresponding row.
#' @examples
#' vic <- mmb::vicinitiesForSample(
#'   df = iris, sampleFromDf = iris[1,], shiftAmount = 0.1)
#' vic$vicinity
#'
#' # Plot the ordered samples to get an idea which ones have a vicinity > 0
#' plot(x=rownames(vic), y=vic$vicinity)
#' @export
vicinitiesForSample <- function(
  df, sampleFromDf,
  selectedFeatureNames = c(),
  shiftAmount = 0.1, doEcdf = FALSE, ecdfMinusOne = FALSE,
  retainMinValues = 0
) {
  if (!is.data.frame(df)) {
    stop("df is not a data.frame.")
  }
  if (!is.data.frame(sampleFromDf) || nrow(sampleFromDf) == 0) {
    stop("sampleFromDf is not a data.frame or empty.")
  }
  if (nrow(sampleFromDf) > 1 && mmb::getWarnings()) {
    warning("More than one sample was given, only taking the first.")
    sampleFromDf <- utils::head(sampleFromDf, 1)
  }

  df <- mmb::bayesConvertData(df)
  samp <- mmb::sampleToBayesFeatures(
    mmb::bayesConvertData(sampleFromDf), colnames(sampleFromDf)[1])

  dfNeighborhood <- mmb::neighborhood(
    df, samp, selectedFeatureNames = selectedFeatureNames,
    retainMinValues = retainMinValues)
  # Names of rows that are NOT in the neighborhood:
  rowsNotNeighborhood <- setdiff(rownames(df), rownames(dfNeighborhood))

  centralities <- mmb::centralities(
    dfNeighborhood, selectedFeatureNames = selectedFeatureNames,
    shiftAmount = shiftAmount, doEcdf = doEcdf, ecdfMinusOne = ecdfMinusOne)

  vic <- data.frame(vicinity = rep(NaN, nrow(df)))
  rownames(vic) <- rownames(df)
  # Let's assign the vicinities of those sample IN the neighborhood:
  vic[names(centralities), ] <- unname(centralities)
  # All other sample not in the neighborhood of the current sample
  # have a vicinity of 0 to it.
  vic[rowsNotNeighborhood, ] <- 0

  return(vic)
}


#' @title Segment a dataset by each row once, then compute vicinities of
#' samples in the neighborhood.
#'
#' @description Given an entire dataset, uses each instance in it to demarcate
#' a neighborhood using the selected features. Then, for each neighborhood,
#' the vicinity of all samples to it is computed. The result of this is an
#' N x N matrix, where the entry \eqn{m_{i,j}} corresponds to the vicinity of
#' sample \eqn{s_j} in neighborhood \eqn{N_i}.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @keywords network
#' @seealso \code{vicinitiesForSample()}
#' @param df data.frame to compute the matrix of vicinites for.
#' @param selectedFeatureNames vector of names of features to use for computing
#' the vicinity/centrality of each sample to each neighborhood.
#' @param shiftAmount numeric DEFAULT 0.1 optional amount to shift each features
#' probability by. This is useful for when the centrality not necessarily must be
#' an actual probability and too many features are selected. To obtain actual
#' probabilities, this needs to be 0, and you must use the ECDF.
#' @param doEcdf boolean DEFAULT FALSE whether to use the ECDF instead of the EPDF
#' to find the likelihood of continuous values.
#' @param ecdfMinusOne boolean DEFAULT FALSE only has an effect if the ECDF is
#' used. If true, uses 1 minus the ECDF to find the probability of a continuous
#' value. Depending on the interpretation of what you try to do, this may be of use.
#' @param retainMinValues DEFAULT 0 the amount of samples to retain during
#' segmentation. For separating a neighborhood, this value typically should
#' be 0, so that no samples are included that are not within it. However,
#' for very sparse data or a great amount of variables, it might still make
#' sense to retain samples.
#' @param useParallel boolean DEFAULT NULL whether to use parallelism or not. Setting this to
#' true requires also having previously registered a parallel backend. If parallel
#' computing is enabled, then each neighborhood is computed separately.
#' @return matrix of length \eqn{N^2} (N being the length of the data.frame). Each
#' row i demarcates the neighborhood as selected by sample i, and each column j then
#' is the vicinity of sample \eqn{s_j} to that neighborhood. No value of the diagonal
#' is zero, because each neighborhood always contains the sample it was demarcated
#' by, and that sample has a similarity greater than zero to it.
#' @examples
#' w <- mmb::getWarnings()
#' mmb::setWarnings(FALSE)
#' mmb::vicinities(df = iris[1:10,])
#'
#' # Run the same, but use the ECDF and retain more values:
#' mmb::vicinities(df = iris[1:10,], doEcdf = TRUE, retainMinValues = 10)
#' mmb::setWarnings(w)
#' @export
vicinities <- function(
  df, selectedFeatureNames = c(),
  shiftAmount = 0.1, doEcdf = FALSE, ecdfMinusOne = FALSE,
  retainMinValues = 0, useParallel = NULL)
{
  if (!is.data.frame(df) || nrow(df) == 0) {
    stop("df is not a data.frame or empty.")
  }

  df <- mmb::bayesConvertData(df)
  len <- nrow(df)
  rns <- rownames(df)

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
  vic <- foreachOpFun(foreach::foreach(
    rn = rns,
    .packages = c("mmb"),
    .combine = cbind
  ), {
    res <- mmb::vicinitiesForSample(
      df, df[rn, ], selectedFeatureNames = selectedFeatureNames,
      shiftAmount = shiftAmount, doEcdf = doEcdf, ecdfMinusOne = ecdfMinusOne,
      retainMinValues = retainMinValues)

    res$vicinity
  })

  if (!is.matrix(vic)) {
    # happens when only one element
    vic <- matrix(vic)
  }

  rownames(vic) <- rns
  colnames(vic) <- rns

  return(vic)
}

