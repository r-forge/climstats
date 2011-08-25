\name{brickstack_to_raster_list}
\alias{brickstack_to_raster_list}
\title{Create a List of Rasters}
\usage{
  brickstack_to_raster_list(brickstack)
}
\arguments{
  \item{brickstack}{A raster brick or raster stack object.}
}
\description{
  Converts a raster stack or a brick into a list of
  individual RasterLayers.
}
\examples{
r <- raster(nrows=10,ncols=10)
r <- setValues(r,1:ncell(r))
#Stack
temp_stack <- stack(r, r/2, r*10)
brickstack_to_raster_list(temp_stack)
#Brick
temp_brick <- brick(r, r/2, r*10)
brickstack_to_raster_list(temp_brick)
}
\author{
  Jonathan A. Greenberg, Alison R. Mynsberge
}
\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
  \code{\link[raster]{stack}}, \code{\link[raster]{brick}}
}
\keyword{brick/stack}

