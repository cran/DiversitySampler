\name{H.sampler}
\alias{H.sampler}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Shannon's Diversity Re-sampler}
\description{
This function is a wrapper for \code{\link{sample}} and can be used to determine the expected value of a diversity index under a specified sampling intensity. A Monte-Carlo procedure is used to re-sample a given observation with replacement at each user-specified sampling intensity. The resampling can take place through one of two schemes. First, where the observed frequencies of species are assumed to represent the true underlying values, and second, where a user-specified vector of probabilities is used to control the resampling. The function calculates a diversity index over each simulated sample and summarises these for each specified level of sampling intensity via their mean.
}
\usage{
H.sampler(x = "community matrix (spp=col,obs=row)", n = "sample size vector", nit = "number of iterations to use", base = exp(1), corr = FALSE, p = NULL, method = "Shannon")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A community matrix with observations in rows and species in columns.}
  \item{n}{A vector or scalar of sample size(s).}
  \item{nit}{Number of iterations of the Monte-Carlo procedure.}
  \item{base}{The base of the logarithm used to calculate Shannon's index (DEFAULT = e).}
  \item{corr}{Correction factor for small sample sizes.}
  \item{p}{A vector or matrix of weights corresponding to x, which can be used to simulate scenarios where forces other than sampling impact diversity.}
  \item{method}{The diversity metric: "Shannon" or "Gene diversity"}
  }

\details{
The Monte-Carlo procedure draws random individuals based on the relative abundances of the species in a given observation. This process is repeated for the number of specified iterations at each designated sample level. For each iteration, the diversity index is calculated, and the mean is taken from all the indices produced in each iteration.
}
\value{
A matrix of diversity indices for each observation (rows) at each sampling level (columns).
}
%\references{}
\author{Matthew K. Lau and Raj Whitlock}
%\note{}
\seealso{ \code{\link{sample}}, \code{\link{Gd}}, \code{\link{Hs}}, \code{\link{count}}}
\examples{
x=array(round(runif(100,1,10)),c(10,10))
H.sampler(x,n=1:10,nit=10,base=exp(1))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{iteration}
\keyword{datagen}
