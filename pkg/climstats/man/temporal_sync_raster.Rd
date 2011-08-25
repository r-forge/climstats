\name{temporal_sync_raster}
\alias{temporal_sync_raster}
\title{Temporally Sync Rasters}
\usage{
  temporal_sync_raster(unsynced, reference, synctype =
  "by.month", unsynced_dates, reference_dates, verbose =
  FALSE)
}
\arguments{
  \item{unsynced}{A raster object or a string pointing to a
  raster on disk to be temporally synced to a reference
  file.}

  \item{reference}{A raster object with reference zvalue
  dates specific to the month and year.}

  \item{synctype}{Type of temporal sync; options are
  "by.month" or "by.year.month".}

  \item{unsynced_dates}{Required if unsynced}

  \item{reference_dates}{Required if reference}

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
pptTahoe
}
\author{
  Jonathan A. Greenberg
}
\seealso{
  \code{\link[climstats]{spatial_sync_raster}}
}
\keyword{brick/stack}
\keyword{calculate}

