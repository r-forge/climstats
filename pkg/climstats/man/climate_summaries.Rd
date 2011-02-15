\name{climate_summaries}
\alias{climate_summaries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Summarize Climate Data }
\description{
Summarizes climate data over desired date ranges, across monthly, annual, or full time intervals, and
(optionally) across periods defined by other climate variables.
}
\usage{
climate_summaries(climate_data,date_range,summary_type,summary_interval="all",apply_maxmin,verbose=FALSE,probs,tie_value="random")
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
		"min","max","sum","mean","sd","cv","spi","quantile").
}
  \item{summary_interval}{
	The interval over which values are returned (options are "all", "monthly", "yearly")
}
  \item{apply_maxmin}{
	A climate data raster containing values to be summarized and returned if which.min.simple or which.max.simple is summary method. 
}
  \item{verbose}{
	verbose=TRUE will print process information.
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
Jonathan A. Greenberg
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
#Use example file for Tahoe, California, USA
require(R.utils)
require(climstats)

load(system.file("extdata/pptTahoe.RData",package="climstats"))
pptTahoe@zvalue
#Total precipitation for 1992
date_range_92=c("1992-01-01","1992-12-31")
ppt_92=climate_summaries(climate_data=pptTahoe,date_range=date_range_92,
	summary_type="sum",summary_interval="yearly")
date_range_9293=c("1992-01-01","1993-12-31")
ppt_9293=climate_summaries(climate_data=pptTahoe,date_range=date_range_9293,
	summary_type="sum",summary_interval="yearly")
ppt_9293
#Total precipitation was lower in 1992 than in 1993

#Now get average annual precipitation totals
ppt_mean_all=climate_summaries(climate_data=ppt_9293,date_range=date_range_9293,
	summary_type="mean",summary_interval="all")	

\dontrun{
# Download and post-process PRISM monthly precipitation data
		ppt = get_climate_data("PRISM-4km-ppt", date_range = c("1999/1/1", "2000/12/31"),
			standardize = TRUE, overwrite = FALSE, enable_download = T, verbose = T)
		summary(ppt)
		ppt@zvalue
# Annual Precipitation		
		date_range=c("1999-01-01","2000-12-31")
		ppt_month_mean=climate_summaries(climate_data=ppt,date_range=date_range,summary_type="mean",summary_interval="all")
		ppt_mean_all=12*ppt_month_mean	#This is an alternative to using the "sum" function
		
# Monthly Precipitation
		ppt_mean_monthly=climate_summaries(ppt,date_range=date_range,summary_type="mean",summary_interval="monthly")
		
# Precipitation of Wettest Month
		monthly_date_range=c("9999-01-01","9999-12-31")
		ppt_of_max_ppt_mean_monthly=climate_summaries(climate_data=ppt_mean_monthly,date_range=monthly_date_range,
				summary_type="which.max.simple",summary_interval="all",apply_maxmin=ppt_mean_monthly)
		
# Precipitation of Driest Month			
		ppt_of_min_ppt_mean_monthly=climate_summaries(climate_data=ppt_mean_monthly,date_range=monthly_date_range,
			summary_type="which.min.simple",summary_interval="all",apply_maxmin=ppt_mean_monthly)
		
# Precipitation Seasonality (Coefficient of Variation)
		ppt_mean_monthly_cv_all=climate_summaries(climate_data=ppt_mean_monthly,date_range=monthly_date_range,summary_type="cv",summary_interval="all")
	
# Min standard precipitation index (SPI)
		ppt_spi_all=climate_summaries(climate_data=ppt,date_range=date_range,summary_type="spi",summary_interval="all")
		ppt_spi_all@zvalue=ref@zvalue
		ppt_spi_all_min=climate_summaries(climate_data=ppt_spi_all,date_range=date_range,summary_type="min",summary_interval="all")
		
# Max standard precipitation index (SPI)
		ppt_spi_all_max=climate_summaries(climate_data=ppt_spi_all,date_range=date_range,summary_type="max",summary_interval="all")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
