\name{eto}
\alias{eto}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Evapotranspiration }
\description{
TBD
}
\usage{
eto(tavg,tmin,tmax,rad,elev,wnd,G=0.0,a=0.23,model="pm-fao")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tavg}{
Average temperature (deg C).  If not assigned, tavg will be calculated as the median of tmax and tmin.
}
  \item{tmin}{
Minimum temperature (deg C).
}
  \item{tmax}{
Maximum temperature (deg C).
}
  \item{rad}{
Net radiation (MJ m-2 day-1).
}
  \item{elev}{
Elevation (m above sea level).
}
  \item{wnd}{
Windspeed (m/s at 2m above terrain).
}
  \item{G}{
Soil heat flux density [MJ m-2 day-1]. By default equals 0.0.
}
  \item{a}{
Reference albedo (unitless, ranges between 0 and 1).  By default equals 0.23.
}
  \item{model}{
Evapotranspiration model to use:
\itemize{
	\item "pm-fao": FAO Penman-Monteith
}
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
