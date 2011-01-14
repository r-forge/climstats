\name{climate_summaries}
\alias{climate_summaries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Climate Summaries }
\description{
Summarizes climate data.
}
\usage{
climate_summaries(climate_data,date_range,summary_type,summary_interval="all",apply_maxmin,verbose=FALSE,probs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{climate_data}{
	A climate data raster to which summary_type is applied; summaries of these values are returned except when the summary method is which.min.simple or which.max.simple.
}
  \item{date_range}{
	The date range over which climate_data should be summarized.
}
  \item{summary_type}{
	The type of summary applied to climate_data (options are "which.min.simple", "which.max.simple",
		"mean","min","max","sd","cv","spi","quantile").
}
  \item{summary_interval}{
	The interval over which values are returned (options are "all", "monthly", "yearly")
}
  \item{apply_maxmin}{
	A climate data raster containing values to be summarized and returned if which.min.simple or which.max.simple is summary method. 
}
  \item{verbose}{
	Logical controlling display of output.
}
  \item{probs}{
	Numeric vector of probabilities with values in [0,1] used by summary_type=quantile.
}
  \item{tie_value}{
	Controls how non-unique extreme values are accommodated by the summary method when summary_type is which.min.simple or which.max.simple.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	Returns a raster layer.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{which.min.simple}}
\code{\link{which.max.simple}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
