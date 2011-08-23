\name{which.min.simple}
\alias{which.min.simple}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Location of Minimum Value }
\description{
Locates the smallest value of the input object.
}
\usage{
which.min.simple(x,na.rm=TRUE,tie_value="random")
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

\value{
An integer of length 1 giving the index of the minimum of x 
or NA if the minimum of x is not unique, x has no non-NAs, or na.rm=F.
}

\author{
Jonathan A. Greenberg, Alison R. Mynsberge
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[base]{which.min}}, 
\code{\link[base]{which}}, 
\code{\link[base]{min}}, 
\code{\link[climstats]{index_raster_to_mask}}
}
\examples{
x<-c(4,1:3,NA,4)
y<-c(2:4,1,1,NA)
## The index is only calculated for a unique minimum
which.min.simple(x)
which.min.simple(y)
which.min.simple(y,na.rm=FALSE)
which.min.simple(x,na.rm=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{calculate}