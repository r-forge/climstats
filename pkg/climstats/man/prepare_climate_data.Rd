\name{prepare_climate_data}
\alias{prepare_climate_data}
\title{Preprocess Climate Data}
\usage{
  prepare_climate_data(raster_files, raster_source, zname,
    zvalue, proj)
}
\arguments{
  \item{raster_files}{needs to be a properly formatted
  search string (?regex) to be used with dir().  You can
  test this by using this command:
  input_files=dir(dirname(raster_files),
  pattern=basename(raster_files),full.names=TRUE)}

  \item{raster_source}{the source of the climate/affiliated
  data.  Currently supports: \itemize{ \item "prism_dem"
  (PRISM digital elevation model \cr
  \url{ftp://prism.oregonstate.edu//pub/prism/us/grids/ \cr
  us_25m_dem.asc.gz}) \item "prism_4km" (PRISM 4km gridded
  climate products) \itemize{ \item precipitation
  (\url{ftp://prism.oregonstate.edu// \cr
  pub/prism/us/grids/ppt/}) \item minimum temperature
  (\url{ftp://prism.oregonstate.edu// \cr
  pub/prism/us/grids/tmin/}) \item maximum temperature
  (\url{ftp://prism.oregonstate.edu// \cr
  pub/prism/us/grids/tmax/}) \item dewpoint temperature
  (\url{ftp://prism.oregonstate.edu// \cr
  pub/prism/us/grids/tdmean/}) \item standardized
  precipitation index (\url{ftp://prism.oregonstate.edu//
  \cr pub/prism/us/grids/spi/}, untested) } \item "narr"
  (North American Regional Reanalysis NetCDF files \cr
  \url{http://www.esrl.noaa.gov/psd/data/gridded/ \cr
  data.narr.html}) \item "generic": function attempts to
  figure it out. This will, in all likelihood, fail.  If
  using generic, you should probably assign the
  zname,zvalue, and proj.}}

  \item{zname}{a string used by raster to denote the
  properties of the z-profile.  This can be anything, but
  there are some special znames that will be used by other
  functions: "Date/time" (the z-values represent time
  series) and "months" (the z-values represent months, so
  there should only be 12 files, in order).}

  \item{zvalue}{if zname="Date/time", zvalue must be a
  character vector equal to the length of the number of
  raster_files found, which is coercable to a date via
  as.Date() using its defaults, "%Y-%m-%d" or "%Y/%m/%d".
  if zname="months", zvalue should be the character vector
  of the integer months of the input files.  If no zvalue
  is given, zvalue will be a vector of 1 to 12.}

  \item{proj}{the projection string in PROJ4 format \cr
  Refer to \url{http://trac.osgeo.org/proj/wiki/GenParms}
  and \url{http://www.remotesensing.org/geotiff/proj_list/}
  \cr Example for lon/lat data: \cr proj="+proj=longlat
  +ellps=WGS84 +datum=WGS84 +no_defs"}
}
\description{
  Assigns zname and zvalues and manages raster projection.
}
\details{
  no details
}
\author{
  Jonathan Greenberg
}
\seealso{
  \code{\link[climstats]{apply_gains_offsets}}
}
\keyword{calculate}
\keyword{climate}

