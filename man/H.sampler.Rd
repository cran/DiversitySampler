\name{H.sampler}
\alias{H.sampler}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Shannon's Diversity Re-sampler}
\description{
 A Monte-Carlo procedure is used to re-sample a given observation at each level of sampling. The expectation being that the mean of the re-sampling will approach Shannon's diversity index at that sample level.
}
\usage{
H.sampler(x = "community matrix (spp=col,obs=row)", n = "sample size vector", nit = "number of iterations to use", base = exp(1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A community matrix with observations in rows and species in columns.}
  \item{n}{A vector or scalar of sample size(s).}
  \item{nit}{Number of iterations of the Monte-Carlo procedure.}
  \item{base}{The base of the logarithm used to calculate Shannon's index (DEFAULT = e).}
}
\details{
The Monte-Carlo procedure draws random individuals based on the relative abundances of the species in a given observation. This process is repeated for the number of specified iterations at each designated sample level. For each iteration, Shannon's diversity index is calculated, and the mean is taken from all the indices produced in each iteration.
}
\value{
A matrix of Shannon's indices for each observation (rows) at each sampling level (columns).
}
\references{}
\author{Matthew K. Lau}
\note{}
\seealso{}
\examples{
x=array(round(runif(100,1,10)),c(10,10))
H.sampler(x,n=1:10,nit=10,base=exp(1))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{iteration}
\keyword{datagen}
