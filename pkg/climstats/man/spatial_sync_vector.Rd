\name{spatial_sync_vector}
\alias{spatial_sync_vector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Spatially Sync Vectors to Rasters }
\description{
Aligns ("syncs") a Vector to a reference Raster. 
}
\usage{
spatial_sync_vector(unsynced,reference,method="ngb",verbose=FALSE)
%,crop_only=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{unsynced}{
A vector object to be aligned to the reference raster.
}
  \item{reference}{
A Raster object to be used as the reference for syncing.  Syncing will use the reference's projection, resolution, and extent.
}
  \item{verbose}{
verbose=TRUE gives feedback on the process (UNSUPPORTED AT PRESENT).
}
%%  \item{crop_only}{
%%crop_only=TRUE will not attempt to expand images, but will only crop them.  If you know your unsynced is fully contained within the reference, use this to speed up the process.
%%}


}
\details{
?spTransform for details on the syncing.
}
\value{
Returns a Vector object (e.g. SpatialPoints, SpatialPointsDataFrame, etc.)
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
\code{\link[climstats]{temporal_sync_raster}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
