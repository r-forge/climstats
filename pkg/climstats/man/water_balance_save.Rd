\name{water_balance_save}
\alias{water_balance_save}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate and Save Water Balance }
\description{
Calculates water balance and saves new raster to a file
}
\usage{
water_balance_save(ppt, eto, model="stephenson", 
	basename="water_balance_climstats_", index_format, 
	output_format="raster",verbose=FALSE,overwrite=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ppt}{
A time series of precipitation amounts (mm H20 per day) in raster format.
}
  \item{eto}{
Evapotranspiration raster
}
  \item{model}{
Currently only supports the Stephenson method for calculating water balance
}
  \item{basename}{
Basename for saved raster
}
  \item{index_format}{
%%     ~~Describe \code{brickstack} here~~
}
  \item{output_format}{
%%     ~~Describe \code{brickstack} here~~
}
  \item{verbose}{
verbose=TRUE will print process information.
}
  \item{overwrite}{
overwrite=TRUE will overwrite raster if already present.
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
}
\keyword{climate}