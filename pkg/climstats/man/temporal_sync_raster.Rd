\name{temporal_sync_raster}
\alias{temporal_sync_raster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Temporally Sync Rasters }
\description{
Aligns ("syncs") a Raster to a reference Raster.
}
\usage{
temporal_sync_raster(unsynced,reference,synctype="by.month",
	unsynced_dates,reference_dates,verbose=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{unsynced}{
A raster object or a string pointing to a raster on disk to be temporally synced to a reference file. 
}
  \item{reference}{
A raster object with reference zvalue dates.
}
  \item{synctype}{
Type of temporal sync; options are "by.month" or "by.year.month".
}
  \item{unsynced_dates}{
Required if unsynced@zvalue does not provide dates for unsynced raster.
}
  \item{reference_dates}{
Required if reference@zvalue does not provide reference dates for reference raster.
}
  \item{verbose}{
verbose=TRUE will print process information.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a RasterLayer, RasterBrick or RasterStack object.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Jonathan A. Greenberg
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[climstats]{spatial_sync_raster}}
}
\examples{
\dontrun{
require(climstats)

load(system.file("extdata/pptTahoe.RData",package="climstats"))
pptTahoe@zvalue

#Download and standardize 2 large monthly wind vector files
wnd=get_climate_data("NARR-monthlymean-wnd",
		date_range=c("1992-01-01","1993-12-31"),
		standardize=TRUE,enable_download=TRUE,verbose=TRUE)
wnd@zvalue

wnd_synced=temporal_sync_raster(wnd,pptTahoe,synctype="by.year.month",
	verbose=TRUE)
wnd_synced@zvalue
}
}
\keyword{calculate}
\keyword{brick/stack}