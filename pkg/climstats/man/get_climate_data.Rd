\name{get_climate_data}
\alias{get_climate_data}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Get/Preprocess Climate Data from Remote Sources }

\description{
This function downloads and preprocesses climate data from a variety of sources.
}
\usage{
get_climate_data(climate_source, date_range, dates, 
	download_folder, final_folder, standardize=TRUE, 
	snow_nthreads=1, overwrite=FALSE,
	wnd_speed_height_correction=TRUE, verbose=FALSE, 
	enable_download=FALSE, local_files, preconvert=TRUE, 
	output_basename="climstats_data", proj, zvalue)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{climate_source}{
A datasource to fetch climate data from.  The following datasources are currently supported:
\itemize{	
	\item "PRISM-4km-ppt": 		PRISM 4km monthly precipitation grids
	\item "PRISM-4km-tmin":		PRISM 4km monthly minimum temperature grids
	\item "PRISM-4km-tmax":		PRISM 4km monthly maximum temperature grids
	\item "PRISM-4km-spi":		PRISM 4km monthly standardized precipitation index grids
	\item "PRISM-4km-tdmean":	PRISM 4km monthly dewpoint grids
	\item "PRISM-4km-elev":	PRISM 4km elevation data
	\item "NARR-monthlymean-wnd":	North American Regional Reanalysis 
		\cr monthly mean wind velocity
	\item "NARR-longtermmonthlymean-wnd":	North American Regional Reanalysis long-term monthly mean wind velocity
	\item "generic": arbitrary local files.  local_files must be set if this is used.
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
standardize=TRUE converts the downloaded dataset to standard units used by eto and water balance 
\itemize{
	\item precipitation: average mm H20 per day
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

  \item{local_files}{
A search string for local files which will be used to create the climate stack when climate_source="generic"
}
  \item{preconvert}{
If preconvert=TRUE (default), raw data will be preconverted to format=raster before any further processing occurs.
}
  \item{output_basename}{
A string representing the output files.  Note that most formats have their own defaults which will override this.
}
  \item{proj}{
A PROJ string which will be used for climate_source="generic".
}
  \item{zvalue}{
The zvalue applied to the output raster (only used with climate_source=
	\cr "generic"). This should be a character string of dates for each of the output layers.  If zvalue="months" and the output is exactly 12 bands, the zvalue will be set to dates corresponding to the first of each month.
}

}

\value{
Returns a RasterLayer, RasterBrick or RasterStack object, as well as saving the objects to disk.
}
\references{
\itemize{
\item PRISM data: \url{http://www.prism.oregonstate.edu/}
\item 	PRISM terms of use: \url{http://www.prism.oregonstate.edu/terms.phtml}
\item	PRISM metadata: \url{http://www.prism.oregonstate.edu/docs/meta}
}
\itemize{ 
\item NCEP North American Regional Reanalysis: \url{http://www.esrl.noaa.gov/psd/
	\cr data/gridded/data.narr.html}
\item 	NCEP NARR Data Description: \url{ftp://ftp.cdc.noaa.gov/Datasets/
	\cr NARR/README} 
}
}
\author{
Jonathan A. Greenberg
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[climstats]{eto}}, 
\code{\link[climstats]{climate_summaries}}
}

\examples{
\dontrun{
# Download and decompress PRISM monthly precipitation data 
# but do not post-process it.
ppt=get_climate_data("PRISM-4km-ppt", 
	date_range=c("1999/1/1","1999/12/31"),
	standardize=FALSE,	enable_download=TRUE)
# Note the raw, decompressed downloaded data.
dir()
# Download and post-process PRISM monthly precipitation data 
# (note that as long as overwrite=FALSE, it will not redownload the files, 
# just post-process the files already acquired during the previous step.
ppt=get_climate_data("PRISM-4km-ppt", 
	date_range=c("1999/1/1","1999/12/31"),
	standardize=TRUE, enable_download=TRUE, overwrite=TRUE)
# Note the raw raster format files (*_raw.grd), and the corrected raster 
# format data (*_climstats.grd).
dir()
summary(ppt)
ppt@zvalue
}
}

\keyword{climate}