\name{get_climate_data}
\alias{get_climate_data}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Get/preprocess climate data from remote sources }

\description{
This function downloads and preprocesses climate data from a variety of sources.
}
\usage{
get_climate_data(climate_source,date_range,dates,download_folder,final_folder,standardize=TRUE,snow_nthreads=1,overwrite=FALSE,wnd_speed_height_correction=TRUE,verbose=FALSE,enable_download=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{climate_source}{
A datasource to fetch climate data from.  The following datasources are currently supported:
\itemize{	
	\item "PRISM-800m-ppt": 		PRISM 800m monthly precipitation grids
	\item "PRISM-800m-tmin":		PRISM 800m monthly minimum temperature grids
	\item "PRISM-800m-tmax":		PRISM 800m monthly maximum temperature grids
	\item "PRISM-800m-spi":		PRISM 800m monthly standardized precipitation index grids
	\item "PRISM-800m-tdmean":	PRISM 800m monthly dewpoint grids
	\item "PRISM-800m-elev":	PRISM 800m elevation data
	\item "NARR-monthlymean-wnd":	North American Regional Reanalysis longterm monthly mean wind velocity
}

}
  \item{date_range}{
A vector of length 2 containing the starting date and ending date of a range of climate data to download.
}
  \item{dates}{
Unused at present.
}
  \item{download_folder}{
The target download directory of the raw data.
}

  \item{final_folder}{
The final directory to store the preprocessed grd files.
}
  \item{standardize}{
standardize=TRUE converts the downloaded dataset to standard units used by climstats 
\itemize{
	\item precipitation: mm H20 per day
	\item temperature: deg C
}
}
  \item{snow_nthreads}{
The number of threads (CPUs) to use when processing.  If snow_nthreads>1, the algorithm will attempt to use the multiple processing package snow via MPI.  CURRENTLY UNSUPPORTED.
}
  \item{overwrite}{
overwrite=TRUE will allow overwriting of downloaded and preprocessed files.  Setting overwrite=FALSE (default) lets the process pick up where it left off if interrupted.
}
  \item{wnd_speed_height_correction}{
wnd_speed_height_correction=TRUE (default) corrects wind speed data to wind speed at 2m above the terrain using the wind profile relationship described in http://www.fao.org/docrep/x0490e/x0490e07.htm equation 47.
}

  \item{verbose}{
verbose=TRUE will print process information.
}
  \item{enable_download}{
If enable_download=TRUE (default), data will be downloaded, otherwise download will be skipped.
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a RasterLayer, RasterBrick or RasterStack object, as well as saving the objects to disk.
}
\references{
\itemize{
\item PRISM data: \url{http://www.prism.oregonstate.edu/}
\item NCEP North American Regional Reanalysis: \url{http://www.esrl.noaa.gov/psd/data/gridded/data.narr.html}
}
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
# Download and decompress PRISM monthly precipitation data but do not post-process it.
ppt=get_climate_data("PRISM-800m-ppt",date_range=c("2000/1/1","2001/12/31"),standardize=FALSE)
# Note the raw, decompressed downloaded data.
dir()
# Download and post-process PRISM monthly precipitation data (note that as long as overwrite=FALSE, it will not
#	redownload the files, just post-process the files already aquired during the previous step.
ppt=get_climate_data("PRISM-800m-ppt",date_range=c("2000/1/1","2001/12/31"),standardize=TRUE,overwrite=FALSE)
# Note the raw raster format files (*_raw.grd), and the corrected raster format data (*_climstats.grd).
dir()
summary(ppt)
ppt@zvalue

}

\keyword{ spatial }
