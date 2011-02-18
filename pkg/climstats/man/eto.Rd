\name{eto}
\alias{eto}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate Evapotranspiration }
\description{
Calculate reference evapotranspiration (ETO) using raster inputs.
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
Calculates evapotranspiration (mm/day) for a reference surface approximating actively growing green grass.  
}
\value{
Returns a raster object of reference evapotranspiration (mm/day). 
}
\references{
Allen, R.G., L.S. Pereira, D. Raes, and M. Smith. 1998. Crop evapotranspiration: guidelines for computing crop water requirements. Food and Agriculture Organization of the United Nations, Rome, Italy.
\url{http://www.fao.org/docrep/x0490e/x0490e00.htm}
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get_climate_data}}, 
\code{\link{spatial_sync_raster}}, 
\code{\link{temporal_sync_raster}}, 
\code{\link{eto_save}}
}
\examples{
\dontrun{
# Requires first downloading and preparing all required inputs using 
#	get_climate_data, spatial_sync_raster, and temporal_sync_raster.
			eto = eto_save(tmin=standardized_tmin,
						tmax=standardized_tmax,
						rad=standardized_rad,
						elev=standardized_elev,
						wnd=standardized_wnd,
						"eto_pm_climstats_",index_format="\%Y-\%m-\%d"
						output_format="raster",verbose=TRUE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{climate}
