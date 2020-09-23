#' @title Discretize a continuous random variable to ranges/buckets.
#'
#' @description Discretizes a continuous random variable into buckets (ranges).
#' Each range is delimited by an exclusive minimum value and an inclusive maximum value.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @keywords discretization
#' @param data a vector with numeric data
#' @param openEndRanges boolean default True. If true, then the minimum value
#' of the first range will be set to @seealso \code{.Machine$double.xmin} and
#' the maximum value of the last range will be set to @seealso \code{.Machine$double.xmax},
#' so that all values get covered.
#' @param numRanges integer default NA. If NULL, then the amount of ranges
#' (buckets) depends on the amount of data given. A minimum of two buckets is
#' used then, and a maximum of ceiling(log2(length(data))).
#' @param exclMinVal numeric default NULL. Used to delimit the lower bound of
#' the given data. If not given, then no value is excluded, as the exclusive
#' lower bound becomes the minimum of the given data minus an epsilon of 1e-15.
#' @param inclMaxVal numeric default NULL. Used to delimit the upper bound of
#' the given data. If not given, then the upper inclusive bound is the max of
#' the given data.
#' @return List a List of vectors, where each vector has two values, the first
#' being the exclusive minimum value of the range, and the second being the
#' inclusive maximum value of the range. The list will be as long as the number
#' of buckets requested.
#' @examples
#' buckets <- mmb::discretizeVariableToRanges(
#'   data = iris$Sepal.Length, openEndRanges = TRUE)
#'
#' length(buckets)
#' buckets[[5]]
#' @export
discretizeVariableToRanges <- function(
  data, openEndRanges = TRUE, numRanges = NA,
  exclMinVal = NULL, inclMaxVal = NULL)
{
  dataNAs <- is.na(data) | is.nan(data)
  hasNAs <- sum(dataNAs) > 0
  if (hasNAs) {
    if (mmb::getWarnings()) {
      warning("Data contains NAs, removing them.")
    }
    data <- data[!dataNAs]
  }

  # Now we either need data or ranges.
  hasData <- is.numeric(data) && length(data) > 0
  hasLimits <- is.numeric(exclMinVal) && is.numeric(inclMaxVal) && exclMinVal < inclMaxVal
  hasRanges <- is.numeric(numRanges) && numRanges > 0

  if (!hasData) {
    # That's fine, but then we need limits and ranges
    if (!hasLimits) stop("No data and no limits given.")
    if (!hasRanges) stop("No data and no ranges given.")

    data <- c(exclMinVal, inclMaxVal)
    exclMinVal <- exclMinVal - 1e-15
  } else {
    if (length(data) == 1 && mmb::getWarnings())
      warning("Only one datapoint given for discretization.")
  }


  if (!hasRanges) {
    numRanges <- max(c(2, ceiling(log2(length(data)))))
  }
  if (!is.numeric(exclMinVal) || is.na(exclMinVal)) {
    exclMinVal <- min(data) - 1e-15 # Do not exclude anything
  }
  if (!is.numeric(inclMaxVal) || is.na(inclMaxVal)) {
    inclMaxVal <- max(data)
  }


  dataCut <- data[data > exclMinVal & data <= inclMaxVal]
  tempRange <- range(dataCut, na.rm = TRUE)
  dataRange <- tempRange[2] - tempRange[1]
  dataStep <- dataRange / numRanges


  allRanges <- list()
  intStart <- exclMinVal
  intStop <- intStart + dataStep
  for (i in 1:numRanges) {
    # Generate an interval (intStart, intStop]
    allRanges[[i]] <- c(intStart, intStop)

    intStart <- intStart + dataStep
    intStop <- intStop + dataStep
  }

  # Let's correct the first and last bucket if open end:
  if (openEndRanges) {
    allRanges[[1]][1] <- -1 * .Machine$double.xmax
    allRanges[[numRanges]][2] <- .Machine$double.xmax
  } else {
    # Because of rounding (dataStep), the inclMax may be smaller
    # than the actual max, so we correct it!
    allRanges[[numRanges]][2] <- inclMaxVal
  }

  return(allRanges)
}



#' @title Get the range-/bucket-ID of a given value.
#'
#' @description Given a list of previously computed ranges for a random
#' variable, this function returns the index of the range the given value
#' belongs to (i.e., in which bucket it belongs). The indexes start R-typically
#' at 1. Per definition, a value is within a range, if it is larger than the
#' range's minimum and less than or equal to its maximum.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @keywords discretization
#' @param ranges list of ranges, as obtained by @seealso \code{discretizeVariableToRanges}
#' @param value numeric a value drawn from the previously discretized
#' random variable.
#' @return integer the index of the range the given value falls into.
#' @examples
#' buckets <- mmb::discretizeVariableToRanges(
#'   data = iris$Sepal.Length, openEndRanges = TRUE)
#'
#' mmb::getRangeForDiscretizedValue(
#'   ranges = buckets, value = mean(iris$Sepal.Length))
#' @export
getRangeForDiscretizedValue <- function(ranges, value) {
  for (i in 1:length(ranges)) {
    valRange <- ranges[[i]] # vector with (min, max]
    if (value > valRange[1] && value <= valRange[2]) {
      return(i)
    }
  }
  stop(paste("The given value", value, "is not within any of the ranges"))
}
