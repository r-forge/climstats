\name{spatial_sync_raster}
\alias{spatial_sync_raster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ the title }
\description{
Aligns ("syncs") a Raster to a reference Raster. 
}
\usage{
spatial_sync_raster(unsynced,reference,method="ngb",verbose=FALSE,crop_only=FALSE)
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
method used to compute values for the new RasterLayer. Either 'ngb' (nearest neighbor) or 'bilinear' (bilinear interpolation).
}
  \item{verbose}{
verbose=TRUE gives feedback on the process (UNSUPPORTED AT PRESENT).
}
  \item{crop_only}{
crop_only=TRUE will not attempt to expand images, but will only crop them.  If you know your unsynced is fully contained within the reference, use this to speed up the process.
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
Jonathan A. Greenberg
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
