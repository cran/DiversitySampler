\name{Hs}
\alias{Hs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Shannon's Diversity Index}
\description{This function allows calculation of Shannon's diversity index (Shannon 1948).

}
\usage{Hs(obs,base=exp(1),corr=FALSE, scrub=TRUE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obs}{A vector of species numbers.}
  \item{base}{The base of the logarithm used to calculate Shannon's index (DEFAULT = e).}
  \item{corr}{Correction factor for small sample sizes.}
  \item{scrub}{Controls the treatment of species with zero abundance in the input observation. If TRUE the input vector is truncated to omit zero values.}
}
\details{This function is used within the \code{\link{H.sampler}} function.
}
\value{
When corr is FALSE the value of scrub will not influence the value returned by this function. When corr is TRUE and scrub is FALSE observations in the input vector that have value 0 will influence the output value through the correction factor -((nsp-1)/(2*nsamp)), where nsp is the number of species and nsamp is the total number of counts. This function is used within the H.sampler function. 
}
\references{Shannon C.E. (1948) A mathematical theory of communication. Bell System Technical Journal 27: 379-423 and 623-656.
}
\author{Raj Whitlock and Matthew K. Lau}
%\note{}
\seealso{\code{\link{H.sampler}}, \code{\link{Gd}}}
\examples{
x=rpois(25,10)
Hs(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{iteration}
\keyword{datagen}

