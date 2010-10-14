\name{which.min.simple}
\alias{which.min.simple}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Location of Minimum Value }
\description{
Locates the smallest value of the input object.
}
\usage{
which.min.simple(x,na.rm=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a numeric object
}
  \item{na.rm}{
a logical indicating whether missing values should be removed.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An integer of length 1 giving the index of the minimum of x 
or NA if the minimum of x is not unique, x has no non-NAs, or na.rm=F.
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
\code{\link[base]{which.min}}, 
\code{\link[base]{which}}, 
\code{\link[base]{min}}
}
\examples{
x<-c(4,1:3,NA,4)
y<-c(2:4,1,1,NA)
## The index is only calculated for a unique minimum
which.min.simple(x)
which.min.simple(y)
which.min.simple(y,na.rm=F)
which.min.simple(x,na.rm=F)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
