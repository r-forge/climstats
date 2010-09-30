\name{list_to_data.frame}
\alias{list_to_data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ the title }
\description{
Takes a list of equal sized objects and returns a data.frame.
}
\usage{
list_to_data.frame(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{brickstack} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (brickstack) 
{
    if ((class(brickstack) != "RasterStack") & (class(brickstack) != 
        "RasterBrick")) {
        print("Input must be a RasterStack or RasterBrick")
        return()
    }
    brickstack_nlayers = nlayers(brickstack)
    brickstack_pos = 1:brickstack_nlayers
    raster_list = mapply(function(brickstack, layer) {
        raster(brickstack, layer = layer)
    }, brickstack_pos, MoreArgs = list(brickstack = brickstack), 
        SIMPLIFY = FALSE)
    return(raster_list)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
