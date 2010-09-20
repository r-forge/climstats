\name{add_leading_zeroes}
\alias{add_leading_zeroes}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Add leading zeroes to a numeric vector }

\description{
Appends leading zeroes to a vector of numbers based on a string length or a maximum number.
}
\usage{
add_leading_zeroes(number,number_length,max_number)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{number}{
A numeric vector.
}
  \item{number_length}{
The length of the output string.
}
  \item{max_number}{
A number to base the length of the output string on.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A character vector.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Jonathan A. Greenberg
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
x=c(1:10)
add_leading_zeroes(x,number_length=4)
add_leading_zeroes(x,max_number=10000)
}

\keyword{ spatial }