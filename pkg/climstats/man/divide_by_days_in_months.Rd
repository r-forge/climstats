\name{divide_by_days_in_months}
\alias{divide_by_days_in_months}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
divide_by_days_in_months(x, dates)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{dates}{
%%     ~~Describe \code{dates} here~~
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
function (x, dates) 
{
    dates_to_days_in_month = function(date_vector) {
        require("chron")
        days_in_months = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 
            31, 30, 31)
        date_vector_chron = as.chron(as.POSIXct(date_vector, 
            origin = "1970-01-01"))
        dates_to_days_in_month = days_in_months[unclass(months(date_vector_chron))]
        dates_to_days_in_month[leap.year(date_vector_chron) & 
            months(date_vector_chron) == "Feb"] <- 29
        return(dates_to_days_in_month)
    }
    if ((class(x) == "RasterStack") | (class(x) == "RasterBrick") | 
        (class(x) == "RasterLayer")) {
        if (missing(dates)) {
            dates = x@zvalue
        }
        days_in_months = dates_to_days_in_month(dates)
        x_divided_by_days_in_months = apply_gains_offsets(x, 
            gains = (1/days_in_months))
    }
    else {
        if (missing(dates)) {
            print("Missing dates...")
            return()
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
