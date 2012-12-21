\name{count}
\alias{count}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Counting Species Abundances}
\description{
Takes a vector of species names and counts the abundance of each unique species. 
}
\usage{
count(sv = "species vector", x = "species sample vector")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sv}{A vector of species numbers or names.}
  \item{x}{A vector of species occurrences.}
}
\details{This function is used within the \code{\link{H.sampler}} function.
}
\value{
Returns a vector of species counts.
}
%\references{}
\author{Matthew K. Lau}
%\note{}
%\seealso{}
\examples{
sv=1:10
x=c(1,1,1,1,1,2,3,3,5,5,5,6,7,8,9,10)
cbind(sv,count(sv,x))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{iteration}
\keyword{datagen}

