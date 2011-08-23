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
A time series of precipitation amounts (mm H2O per day) in raster format.
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


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[climstats]{get_climate_data}}, 
\code{\link[climstats]{eto}}
}
\examples{
}
\keyword{climate}