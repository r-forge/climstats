\name{apply_gains_offsets}
\alias{apply_gains_offsets}
\title{Apply Gains and Offsets}
\usage{
  apply_gains_offsets(x, gains, offsets,
    divide_by_days_in_month = FALSE, snow_nthreads = 1)
}
\arguments{
  \item{x}{An input, such as a numeric vector (UNSUPPORTED)
  or a raster object.}

  \item{gains}{The gain to apply to the input.  gain should
  be either a numeric vector equal to the nlayers(x) (if x
  is a raster) or a single value which will be applied to
  all layers.}

  \item{offsets}{The offset to apply to the input.  gain
  should be either a numeric vector equal to the nlayers(x)
  (if x is a raster) or a single value which will be
  applied to all layers.}

  \item{divide_by_days_in_month}{IF x is a raster, and has
  a Date-coercible zvalue set, setting \cr
  divide_by_days_in_month=TRUE will divide a layer by the
  numbers of days in the month.}

  \item{snow_nthreads}{UNUSED AT PRESENT.}
}
\description{
  Applies linear gains and offsets to input data following
  output = gains * x + offset.
}
\examples{
\dontrun{

#Divide all precipitation values by 10 to get cm H2O
load(system.file("extdata/pptTahoe.RData",package="climstats"))
ppt_cm=apply_gains_offsets(pptTahoe,gains=(1/10),
	divide_by_days_in_month=FALSE)
}
}
\author{
  Jonathan A. Greenberg
}
\seealso{
  \code{\link[climstats]{get_climate_data}}
}
\keyword{calculate}
\keyword{format}

