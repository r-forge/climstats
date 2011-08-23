\name{spatial_sync_raster}
\alias{spatial_sync_raster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Spatially Sync Rasters }
\description{
Aligns ("syncs") a Raster to a reference Raster. 
}
\usage{
spatial_sync_raster(unsynced,reference,method="ngb",verbose=FALSE)
%,crop_only=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{unsynced}{
A Raster object to be aligned to the reference raster.
}
  \item{reference}{
A Raster object to be used as the reference for syncing.  Syncing will use the reference's projection, resolution, and extent.
}
  \item{method}{
Method used to compute values for the new RasterLayer. Either 'ngb' (nearest neighbor) or 'bilinear' (bilinear interpolation).
}
  \item{verbose}{
verbose=TRUE gives feedback on the process (UNSUPPORTED AT PRESENT).
}
%%  \item{crop_only}{
%%crop_only=TRUE will not attempt to expand images, but will only crop them.  If you know your unsynced is fully contained within the reference, use this to speed up the process.
%%}


}
\details{
Uses bilinear or nearest neighbor resampling to align a raster to the extent and projection of a reference raster and match the resolution of the reference raster.
}
\value{
Returns a RasterLayer, RasterBrick or RasterStack object synced to the reference raster object.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Jonathan A. Greenberg
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[climstats]{temporal_sync_raster}}
}
\examples{
\dontrun{
require(R.utils) #Can't be loaded after raster
require(climstats)
load(system.file("extdata/elevTahoe.RData",package="climstats"))

ppt=get_climate_data("PRISM-4km-ppt", 
		date_range=c("1999/1/1","1999/12/31"), standardize=TRUE, 
		enable_download=TRUE, overwrite=TRUE)
		
pptTahoe99=spatial_sync_raster(ppt,elevTahoe,method="bilinear")
pptTahoe99
ppt99jan<-raster(pptTahoe99,layer=1)
plot(ppt99jan)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{brick/stack}
\keyword{calculate}