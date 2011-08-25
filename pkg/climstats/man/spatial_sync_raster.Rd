\name{spatial_sync_raster}
\alias{spatial_sync_raster}
\title{Spatially Sync Rasters}
\usage{
  spatial_sync_raster(unsynced, reference, method = "ngb",
  verbose = FALSE)
}
\arguments{
  \item{unsynced}{A Raster object to be aligned to the
  reference raster.}

  \item{reference}{A Raster object to be used as the
  reference for syncing. Syncing will use the reference's
  projection, resolution, and extent.}

  \item{method}{Method used to compute values for the new
  RasterLayer. Either 'ngb' (nearest neighbor) or
  'bilinear' (bilinear interpolation).}

  \item{verbose}{verbose=TRUE gives feedback on the process
  (UNSUPPORTED AT PRESENT).}
}
\value{
  Returns a RasterLayer, RasterBrick or RasterStack object
  synced to the reference raster object.
}
\description{
  Aligns ("syncs") a Raster to a reference Raster.
}
\details{
  Uses bilinear or nearest neighbor resampling to align a
  raster to the extent and projection of a reference raster
  and match the resolution of the reference raster.
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
\author{
  Jonathan A. Greenberg
}
\seealso{
  \code{\link[climstats]{temporal_sync_raster}}
}
\keyword{brick/stack}
\keyword{calculate}

