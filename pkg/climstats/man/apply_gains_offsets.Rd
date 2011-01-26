\name{apply_gains_offsets}
\alias{apply_gains_offsets}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Apply gains and offsets }


\description{
Applies linear gains and offsets to input data following output = gains * x + offset.
}
\usage{
apply_gains_offsets(x, gains, offsets, divide_by_days_in_month=FALSE,snow_nthreads=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An input, such as a numeric vector (UNSUPPORTED) or a raster object. 
}
  \item{gains}{
The gain to apply to the input.  gain should be either a numeric vector equal to the nlayers(x) (if x is a raster) or a single value which will be applied to all layers.
}
  \item{offsets}{
The offset to apply to the input.  gain should be either a numeric vector equal to the nlayers(x) (if x is a raster) or a single value which will be applied to all layers.
}  \item{divide_by_days_in_month}{
IF x is a raster, and has a Date-coercible zvalue set, setting divide_by_days_in_month=TRUE will divide a layer by the numbers of days in the month.
}  \item{snow_nthreads}{
UNUSED AT PRESENT.
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
\seealso{
\code{\link[climstats]{get_climate_data}}
}
\examples{
#Divide all precipitation values by 100
load(system.file("extdata/pptTahoe.RData",package="climstats"))
ppt_standardized=apply_gains_offsets(pptTahoe,gains=(1/100),
	divide_by_days_in_month=FALSE)
}