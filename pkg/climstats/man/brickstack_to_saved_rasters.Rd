\name{brickstack_to_saved_rasters}
\alias{brickstack_to_saved_rasters}
\title{Save a RasterStack or RasterBrick}
\usage{
  brickstack_to_saved_rasters(brickstack, output_basename,
    use_layernames = FALSE)
}
\arguments{
  \item{brickstack}{A raster brick or raster stack object.}

  \item{output_basename}{A character string for naming the
  objects.}

  \item{use_layernames}{If TRUE, filenames (from disk) are
  appended to output_basename to designate individual
  layers. If FALSE, layers are distinguished on disk based
  on their order in the RasterBrick or RasterStack.}
}
\value{
  Saves rasters to the working directory.
}
\description{
  Saves a raster stack or a brick as individual
  RasterLayers.
}
\examples{
\dontrun{r <- raster(nrows=10,ncols=10)
r <- setValues(r,1:ncell(r))
#Stack
temp_stack <- stack(r, r/2, r*10)
brickstack_to_saved_rasters(temp_brick,output_basename="temp")
#Brick
temp_brick <- brick(r, r/2, r*10)
brickstack_to_saved_rasters(temp_brick,output_basename="bricklayers")}
}
\author{
  Jonathan A. Greenberg
}
\seealso{
  \code{\link[raster]{stack}}, \code{\link[raster]{brick}}
}
\keyword{brick/stack}

