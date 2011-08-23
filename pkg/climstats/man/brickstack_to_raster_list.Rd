\name{brickstack_to_raster_list}
\alias{brickstack_to_raster_list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create a List of Rasters }
\description{
Converts a raster stack or a brick into a list of individual RasterLayers.
}
\usage{
brickstack_to_raster_list(brickstack)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{brickstack}{
A raster brick or raster stack object.
}
}
\author{
Jonathan A. Greenberg, Alison R. Mynsberge
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link[raster]{stack}},  
\code{\link[raster]{brick}} 
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{brick/stack}
