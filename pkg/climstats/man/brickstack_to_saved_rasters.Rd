\name{brickstack_to_saved_rasters}
\alias{brickstack_to_saved_rasters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ the title }
\description{
Saves a raster stack or a brick as individual RasterLayers.
}
\usage{
brickstack_to_saved_rasters(brickstack,output_basename,use_layernames=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{brickstack}{
A raster brick or raster stack object.
}
  \item{output_basename}{
A character string for naming the objects.
}
  \item{use_layernames}{
If TRUE, filenames (from disk) are appended to output_basename to designate individual layers.
If FALSE, layers are distinguished on disk based on their order in the RasterBrick or RasterStack.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Saves rasters to the working directory.}
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
\code{\link[raster]{stack}},\code{\link[raster]{brick}} 
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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
