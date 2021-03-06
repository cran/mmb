% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discretization.R
\name{getRangeForDiscretizedValue}
\alias{getRangeForDiscretizedValue}
\title{Get the range-/bucket-ID of a given value.}
\usage{
getRangeForDiscretizedValue(ranges, value)
}
\arguments{
\item{ranges}{list of ranges, as obtained by @seealso \code{discretizeVariableToRanges}}

\item{value}{numeric a value drawn from the previously discretized
random variable.}
}
\value{
integer the index of the range the given value falls into.
}
\description{
Given a list of previously computed ranges for a random
variable, this function returns the index of the range the given value
belongs to (i.e., in which bucket it belongs). The indexes start R-typically
at 1. Per definition, a value is within a range, if it is larger than the
range's minimum and less than or equal to its maximum.
}
\examples{
buckets <- mmb::discretizeVariableToRanges(
  data = iris$Sepal.Length, openEndRanges = TRUE)

mmb::getRangeForDiscretizedValue(
  ranges = buckets, value = mean(iris$Sepal.Length))
}
\author{
Sebastian Hönel \href{mailto:sebastian.honel@lnu.se}{sebastian.honel@lnu.se}
}
\keyword{discretization}
