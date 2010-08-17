\name{windvectors_to_wind}
\alias{windvectors_to_wind}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
windvectors_to_wind(uwnd, vwnd, wnd_height)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{uwnd}{
%%     ~~Describe \code{uwnd} here~~
}
  \item{vwnd}{
%%     ~~Describe \code{vwnd} here~~
}
  \item{wnd_height}{
%%     ~~Describe \code{wnd_height} here~~
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
function (uwnd, vwnd, wnd_height) 
{
    require(raster)
    if (missing(uwnd) || missing(uwnd)) {
        print("Missing one or both wind vectors, please try again...")
        return()
    }
    if (missing(wnd_height)) {
        wnd_2m_conversion_factor = 1
    }
    else {
        wnd_2m_conversion_factor = 4.87/(log(67.8 * wnd_height - 
            5.42))
    }
    uwnd_list = brickstack_to_raster_list(uwnd)
    vwnd_list = brickstack_to_raster_list(vwnd)
    wnd = stack(mapply(function(uwnd, vwnd, wnd_2m_conversion_factor) {
        ((uwnd^2 + vwnd^2)^(0.5)) * wnd_2m_conversion_factor
    }, uwnd_list, vwnd_list, MoreArgs = list(wnd_2m_conversion_factor = wnd_2m_conversion_factor), 
        SIMPLIFY = FALSE))
    wnd@zvalue = uwnd@zvalue
    wnd@zname = uwnd@zname
    return(wnd)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
