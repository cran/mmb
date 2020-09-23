#' @title Safe PDF estimation that works also for sparse random variables.
#'
#' @description Given a few observations of a random variable, this function
#' returns an approximation of the PDF as a function. Returns also the PDF's
#' support and argmax and works when only zero or one value was given. Depending
#' on the used density function, two values are often enough to estimate a PDF.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @keywords density-estimation likelihood
#' @note If the given vector is empty, warns and returns a constant function
#' that always returns zero for all values.
#' @note If the given vector contains only one observation, then a function
#' is returned that returns 1 iff the value supplied is the same as the
#' observation. Otherwise, that function will return zero.
#' @param data vector of numeric data. Used to compute the empirical density
#' of the data.
#' @param densFun function default \code{stats::density} with bandwith 'SJ'.
#' Function to compute the empirical density of a non-empty vector of numerical
#' data. Note that this function needs to return the properties 'x' and 'y' as
#' \code{stats::density} does, so that we can return the argmax.
#' @return list with a function that is the empirical PDF using KDE. The list
#' also has two properties 'min' and 'max' which represent the integratable
#' range of that function. 'min' and 'max' are both zero if not data (an
#' empty vector) was given. If one data point was given, then they correspond
#' to its value -/+ \code{.Machine$double.eps}. The list further contains two
#' numeric vectors 'x' and 'y', and a property 'argmax'. If no data was given,
#' 'x' and 'y' are zero, and 'argmax' is NA. If one data points was given,
#' then 'x' and 'argmax' equal it, and 'y' is set to 1. If two or more data
#' points given, then the empirical density is estimated and 'x' and y' are
#' filled from its estimate. 'argmax' is then set to that 'x', where 'y'
#' becomes max.
#' @examples
#' epdf <- mmb::estimatePdf(data = iris$Petal.Width)
#' print(epdf$argmax)
#' plot(epdf)
#'
#' # Get relative likelihood of some values:
#' epdf$fun(0.5)
#' epdf$fun(1.7)
#' @export
estimatePdf <- function(data = c(), densFun = function(vec) {
  stats::density(vec, bw = "SJ")
}) {
  l <- length(data)
  pdf <- list(
    fun = function(x) 0,
    min = 0,
    max = 0,
    x = c(),
    y = c(),
    argmax = NaN
  )

  if (l == 0) {
    if (mmb::getWarnings()) warning("No data was given for the PDF.")
  } else if (l == 1) {
    if (mmb::getWarnings()) warning("Only one data point given for estimating the PDF.")
    pdf$fun <- function(x) {
      if (x == data[1]) {
        return(1)
      }
      return(0)
    }
    pdf$min <- data[1] - .Machine$double.eps
    pdf$max <- data[1] + .Machine$double.eps
    pdf$x <- c(data[1])
    pdf$y <- c(1)
    pdf$argmax <- data[1]
  } else {
    tryCatch({
      set.seed(84735)
      f <- densFun(data)
      af <- stats::approxfun(f)
      pdf$fun <- function(x) {
        # If the current value is out of range, then its density is zero anyway, because
        # all of the other values evolve around another range, i.e., the current value
        # is outside the support of the ePDF.
        if (x < pdf$min || x > pdf$max) {
          return(0)
        }
        return(af(x))
      }
      pdf$min <- min(f$x)
      pdf$max <- max(f$x)
      pdf$x <- f$x
      pdf$y <- f$y
      pdf$argmax <- pdf$x[which.max(pdf$y)]
    }, error=function(cond) {
      if (mmb::getWarnings()) {
        warning(paste("Density estimation failed:", cond))
      }
    })
  }

  return(pdf)
}



#' @title Get a probability of a discrete value.
#'
#' @description Similar to @seealso \code{estimatePdf}, this function returns
#' the probability for a discrete value, given some observations.
#'
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @keywords probability likelihood
#' @note If no observations are given, then this function will warn and return
#' a probability of zero for the value given. While we could technically return
#' positive infinity, 0 is more suitable in the context of Bayesian inferencing.
#' @param data vector of observations that have the same type as the given value.
#' @param value a single observation of the same type as the data vector.
#' @return the probability of value given data.
#' @examples
#' mmb::getProbForDiscrete(data = c(), value = iris[1,]$Species)
#' mmb::getProbForDiscrete(data = iris$Species, value = iris[1,]$Species)
#' @export
getProbForDiscrete <- function(data, value) {
  if (length(data) == 0) {
    if (mmb::getWarnings()) warning("No data was given for calculating probability for discrete value.")
    return(0)
  }

  return(sum(data == value) / length(data))
}
