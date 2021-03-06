% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayesSimple.R
\name{bayesProbabilitySimple}
\alias{bayesProbabilitySimple}
\title{Assign a probability using a simple (network) Bayesian classifier.}
\usage{
bayesProbabilitySimple(
  df,
  features,
  targetCol,
  selectedFeatureNames = c(),
  retainMinValues = 1,
  doEcdf = FALSE
)
}
\arguments{
\item{df}{data.frame}

\item{features}{data.frame with bayes-features. One of the features needs to
be the label-column.}

\item{targetCol}{string with the name of the feature that represents the label.}

\item{selectedFeatureNames}{vector default \code{c()}. Vector of strings that
are the names of the features the to-predict label depends on. If an empty
vector is given, then all of the features are used (except for the label). The
order then depends on the features' order.}

\item{retainMinValues}{integer to require a minimum amount of data points
when segmenting the data feature by feature.}

\item{doEcdf}{default FALSE a boolean to indicate whether to use the
empirical CDF to return a probability when inferencing a continuous
feature. If false, uses the empirical PDF to return the rel. likelihood.}
}
\value{
double the probability of the target-label, using the maximum a
posteriori estimate.
}
\description{
Uses simple Bayesian inference to return the probability or
relative likelihood or a discrete label or continuous value.
}
\examples{
feat1 <- mmb::createFeatureForBayes(
  name = "Sepal.Length", value = mean(iris$Sepal.Length))
feat2 <- mmb::createFeatureForBayes(
  name = "Sepal.Width", value = mean(iris$Sepal.Width), isLabel = TRUE)

# Assign a probability to a continuous variable (also works with nominal):
mmb::bayesProbabilitySimple(df = iris, features = rbind(feat1, feat2),
  targetCol = feat2$name, retainMinValues = 5, doEcdf = TRUE)
}
\references{
\insertRef{rpkg:bnlearn_4.5}{mmb}
}
\seealso{
\code{mmb::bayesInferSimple()}
}
\author{
Sebastian Hönel \href{mailto:sebastian.honel@lnu.se}{sebastian.honel@lnu.se}
}
\keyword{inferencing}
\keyword{simple}
