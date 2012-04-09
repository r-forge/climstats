\name{temporal_sync_raster}
\alias{temporal_sync_raster}
\title{Temporally Sync Rasters}
\usage{
  temporal_sync_raster(unsynced, reference,
    synctype = "by.month", unsynced_dates, reference_dates,
    verbose = FALSE)
}
\arguments{
  \item{unsynced}{A raster object or a string pointing to a
  raster on disk to be temporally synced to a reference
  file.}

  \item{reference}{A raster object with reference zvalue
  dates specific to the month and year.}

  \item{synctype}{Type of temporal sync; options are
  "by.month" or "by.year.month".}

  \item{unsynced_dates}{Required if getZ(unsynced) does not
  provide dates for unsynced raster.}

  \item{reference_dates}{Required if getZ(reference) does
  not provide reference dates for reference raster.}

  \item{verbose}{verbose=TRUE will print process
  information.}
}
\value{
  Returns a RasterLayer, RasterBrick or RasterStack object.
}
\description{
  Aligns ("syncs") a Raster to a reference Raster.
}
\details{
  Matches the temporal resolution and extent of the
  unsynced raster object to the reference raster object.
  Files with 12 layers (1/month) are expanded to multiple
  years using "by.month". Files with unique entries per
  month per year are reduced to the temporal range of the
  reference object using "by.year.month".
}
\examples{
\dontrun{
require(climstats)

load(system.file("extdata/pptTahoe.RData",package="climstats"))
getZ(pptTahoe)

#Download and standardize 2 large monthly wind vector files
wnd=get_climate_data("NARR-monthlymean-wnd",
		date_range=c("1992-01-01","1993-12-31"),
		standardize=TRUE,enable_download=TRUE,verbose=TRUE)
getZ(wnd)

wnd_synced=temporal_sync_raster(wnd,pptTahoe,synctype="by.year.month",
	verbose=TRUE)
getZ(wnd_synced)
}
}
\author{
  Jonathan A. Greenberg
}
\seealso{
  \code{\link[climstats]{spatial_sync_raster}}
}
\keyword{brick/stack}
\keyword{calculate}

