\name{climate_summaries}
\alias{climate_summaries}
\title{Summarize Climate Data}
\usage{
  climate_summaries(climate_data, date_range, summary_type,
  summary_interval = "all", apply_maxmin, verbose = FALSE,
  probs, tie_value = "random")
}
\arguments{
  \item{climate_data}{A climate data raster to which
  summary_type is applied; summaries of these values are
  returned except when the summary method is
  which.min.simple or which.max.simple.}

  \item{date_range}{The date range over which climate_data
  should be summarized.}

  \item{summary_type}{The type of summary applied to
  climate_data (options are "which.min.simple",
  "which.max.simple",
  "min","max","sum","mean","sd","cv","spi","quantile").}

  \item{summary_interval}{The interval over which values
  are returned (options are "all", "monthly", \cr
  "yearly")}

  \item{apply_maxmin}{A climate data raster containing
  values to be summarized and returned if \cr
  which.min.simple or which.max.simple is summary method.}

  \item{verbose}{verbose=TRUE will print process
  information.}

  \item{probs}{Numeric vector of probabilities with values
  in [0,1] used by \cr summary_type=quantile.}

  \item{tie_value}{Controls how non-unique extreme values
  are accommodated by the summary method when summary_type
  is which.min.simple or which.max.simple.}
}
\value{
  Returns a raster layer.
}
\description{
  Summarizes climate data over desired date ranges, across
  monthly, annual, or full time intervals, and (optionally)
  across periods defined by other climate variables.
}
\examples{
# Use example file for Tahoe, California, USA
require(R.utils)
require(climstats)

load(system.file("extdata/pptTahoe.RData",package="climstats"))
# View dates of precipitation file
pptTahoe
}
\author{
  Jonathan A. Greenberg
}
\seealso{
  \code{\link[climstats]{get_climate_data}},
  \code{\link[climstats]{eto}},
  \code{\link{which.min.simple}},
  \code{\link{which.max.simple}}
}
\keyword{climate}

