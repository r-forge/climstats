\name{list_to_data.frame}
\alias{list_to_data.frame}
\title{Convert a List to a Data Frame}
\usage{
  list_to_data.frame(x)
}
\arguments{
  \item{x}{a list of vectors of equal length}
}
\value{
  A data frame with listed vectors as rows.
}
\description{
  Takes a list of equal-sized objects and returns a data
  frame.
}
\examples{
\dontrun{

x<-c(4,1:3,NA,4)
y<-c(2:4,1,1,NA)
length(x)==length(y)
list_to_data.frame(list(x,y))
}
}
\author{
  Jonathan A. Greenberg, Alison R. Mynsberge
}
\keyword{format}

