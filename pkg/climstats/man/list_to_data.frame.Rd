\name{list_to_data.frame}
\alias{list_to_data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Convert a List to a Data Frame }
\description{
Takes a list of equal-sized objects and returns a data frame.
}
\usage{
list_to_data.frame(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a list of vectors of equal length
}
}

\value{
A data frame with listed vectors as rows.
}

\author{
Jonathan A. Greenberg, Alison R. Mynsberge
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x<-c(4,1:3,NA,4)
y<-c(2:4,1,1,NA)
length(x)==length(y)
list_to_data.frame(list(x,y))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{format}