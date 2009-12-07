\name{Gd}
\alias{Gd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Genetic Evenness Index}
\description{This function allows calculation of a measure of compositional evenness, e.g. for a community of species or a population of genotypes (genetic individuals). Specifically, the index of evenness gives a measure of the probability that when two individuals are drawn randomly from the community (population) these individuals are of differing type (species or genotype; Nei 1987).

}
\usage{Gd(obs,scrub=FALSE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obs}{A vector of species numbers.}
  \item{scrub}{Controls the treatment of species with zero abundance in the input observation. If TRUE the input vector is truncated to omit zero values.}
}
\details{Used within the \code{\link{H.sampler}} function.
}
\value{
Returns the compositional evenness calculated from the sample.
}
\references{Nei M. 1987. Molecular Evolutionary Genetics. Columbia University Press, New York, p. 180. 
}
\author{Raj Whitlock}
\note{}
\seealso{\code{\link{H.sampler}}, \code{\link{Hs}}}
\examples{
x=rpois(25,10)
Gd(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{iteration}
\keyword{datagen}

