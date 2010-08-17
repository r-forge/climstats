\name{apply_gains_offsets}
\alias{apply_gains_offsets}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
apply_gains_offsets(x, gains, offsets)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{gains}{
%%     ~~Describe \code{gains} here~~
}
  \item{offsets}{
%%     ~~Describe \code{offsets} here~~
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
function (x, gains, offsets) 
{
    if ((class(x) == "RasterStack") | (class(x) == "RasterBrick") | 
        (class(x) == "RasterLayer")) {
        x_nlayers = nlayers(x)
        if (missing(gains)) {
            gains = rep(1, x_nlayers)
        }
        if (missing(offsets)) {
            offsets = rep(0, x_nlayers)
        }
        x_list = brickstack_to_raster_list(x)
        x_gains_offsets = stack(mapply(function(x, gains, offsets) {
            x * gains + offsets
        }, x_list, gains, offsets, SIMPLIFY = FALSE))
        x_gains_offsets@zvalue = x@zvalue
        x_gains_offsets@zname = x@zname
    }
    else {
        x_gains_offsets = x * gains + offsets
    }
    return(x_gains_offsets)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
