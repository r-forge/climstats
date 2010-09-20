\name{get_climate_data}
\alias{get_climate_data}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Get/preprocess climate data from remote sources }

\description{
This function downloads and preprocesses climate data from a variety of sources.
}
\usage{
get_climate_data(climate_source,dates,startdate,enddate,download_folder,final_folder,standardize=TRUE,snow_nthreads=1,overwrite=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{climate_source}{
A datasource to fetch climate data from.  The following datasources are currently supported:
	"PRISM-800m-ppt": 		PRISM 800m monthly precipitation grids
	"PRISM-800m-tmin":		PRISM 800m monthly minimum temperature grids
	"PRISM-800m-tmax":		PRISM 800m monthly maximum temperature grids
	"PRISM-800m-spi":		PRISM 800m monthly standardized precipitation index grids
	"PRISM-800m-tdmean":	PRISM 800m monthly dewpoint grids
}
  \item{dates}{
A vector of dates to download.
}
  \item{startdate}{
The starting date of the range of climate data to download.
}
  \item{enddate}{
The ending date of the range of climate data to download.
}
  \item{download_folder}{
The target download directory of the raw data.
}
  \item{final_folder}{
The final directory to store the preprocessed grd files.
}
  \item{standardize}{
standardize=TRUE converts the downloaded dataset to standard units used by climstats (e.g. for precipitation: mm H20 per day, temperature: deg C).
}
  \item{snow_nthreads}{
The number of threads (CPUs) to use when processing.  If snow_nthreads>1, the algorithm will attempt to use the multiple processing package snow via MPI.  
}
  \item{overwrite}{
overwrite=TRUE will allow overwriting of downloaded and preprocessed files.
}


}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list of rasterLayers.
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
%% TBD

}

\keyword{ spatial }
