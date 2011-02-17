\name{which.max.simple}
\alias{which.max.simple}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Location of Maximum Value }
\description{
Locates the largest value of the input object.
}
\usage{
which.max.simple(x,na.rm=TRUE,tie_value="random")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a numeric object
}
  \item{na.rm}{
a logical indicating whether missing values should be removed.
}
  \item{tie_value}{
A character indicating how to deal with ties.  Can be "NA" (returns an NA if a tie is found) or "random" (returns a single randomly chosen member of the ties if a tie is found).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An integer of length 1 giving the index of the maximum of x 
or NA if the maximum of x is not unique, x has no non-NAs, or na.rm=F.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Jonathan A. Greenberg, Alison R. Mynsberge
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[base]{which.max}}, 
\code{\link[base]{which}}, 
\code{\link[base]{max}}
}
\examples{
x<-c(2:4,1,1,NA)
y<-c(4,1:3,NA,4)
## The index is only calculated for a unique maximum
which.max.simple(x)
which.max.simple(y)
which.max.simple(y,na.rm=FALSE)
which.max.simple(x,na.rm=FALSE)
}
\keyword{calculate}