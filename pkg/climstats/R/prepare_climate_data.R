#' Preprocess Climate Data
#' 
#' Assigns zname and zvalues and manages raster projection.
#' 
#' no details
#' 
#' @param raster_files needs to be a properly formatted search string (?regex)
#' to be used with dir().  You can test this by using this command:
#' input_files=dir(dirname(raster_files),
#' pattern=basename(raster_files),full.names=TRUE)
#' @param raster_source the source of the climate/affiliated data.  Currently
#' supports: \itemize{ \item "prism_dem" (PRISM digital elevation model \cr
#' \url{ftp://prism.oregonstate.edu//pub/prism/us/grids/ \cr
#' us_25m_dem.asc.gz}) \item "prism_4km" (PRISM 4km gridded climate products)
#' \itemize{ \item precipitation (\url{ftp://prism.oregonstate.edu// \cr
#' pub/prism/us/grids/ppt/}) \item minimum temperature
#' (\url{ftp://prism.oregonstate.edu// \cr pub/prism/us/grids/tmin/}) \item
#' maximum temperature (\url{ftp://prism.oregonstate.edu// \cr
#' pub/prism/us/grids/tmax/}) \item dewpoint temperature
#' (\url{ftp://prism.oregonstate.edu// \cr pub/prism/us/grids/tdmean/}) \item
#' standardized precipitation index (\url{ftp://prism.oregonstate.edu// \cr
#' pub/prism/us/grids/spi/}, untested) } \item "narr" (North American Regional
#' Reanalysis NetCDF files \cr \url{http://www.esrl.noaa.gov/psd/data/gridded/
#' \cr data.narr.html}) \item "generic": function attempts to figure it out.
#' This will, in all likelihood, fail.  If using generic, you should probably
#' assign the zname,zvalue, and proj.}
#' @param zname a string used by raster to denote the properties of the
#' z-profile.  This can be anything, but there are some special znames that
#' will be used by other functions: "Date/time" (the z-values represent time
#' series) and "months" (the z-values represent months, so there should only be
#' 12 files, in order).
#' @param zvalue if zname="Date/time", zvalue must be a character vector equal
#' to the length of the number of raster_files found, which is coercable to a
#' date via as.Date() using its defaults, "%Y-%m-%d" or "%Y/%m/%d".  if
#' zname="months", zvalue should be the character vector of the integer months
#' of the input files.  If no zvalue is given, zvalue will be a vector of 1 to
#' 12.
#' @param proj the projection string in PROJ4 format \cr Refer to
#' \url{http://trac.osgeo.org/proj/wiki/GenParms} and
#' \url{http://www.remotesensing.org/geotiff/proj_list/} \cr Example for
#' lon/lat data: \cr proj="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#' @author Jonathan Greenberg
#' @seealso \code{\link[climstats]{apply_gains_offsets}}
#' @keywords calculate climate
#' 
#' 
prepare_climate_data=function(raster_files,raster_source,zname,zvalue,proj)
{
	# Raster files should be the full path + search string for the file(s) which will be merged into a raster stack.
	require(raster)
	input_files=dir(dirname(raster_files),
			pattern=basename(raster_files),
			full.names=TRUE)
	input_files_N=length(input_files)

	days_in_months=c(31,28,31,30,31,30,31,31,30,31,30,31)
	middays_in_months=ceiling(days_in_months/2)
	
	if (input_files_N==0)
	{
		print("No files found, exiting")
		return()
	}
	
	if (raster_source=="prism_dem")
	{
		raster_object_from_files=raster(sort(input_files))
		projection(raster_object_from_files)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
		raster_object_from_files@zname="elev"
	}
	
	if (raster_source=="prism_800m"|raster_source=="prism_4km")
	{
		raster_object_from_files=stack(sort(input_files))
		projection(raster_object_from_files)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
		
		# Dates are embedded in the filenames.
		dates=vector("character",length=input_files_N)
		for (j in (1:input_files_N))
		{
			temp_date_vector=as.numeric(strsplit(strsplit(basename(input_files[j]),"_")[[1]][3],"[.]")[[1]])
			if (temp_date_vector[2] >= 1 & temp_date_vector[2] <=12)
			{
				dates[j]=paste(temp_date_vector[1],temp_date_vector[2],middays_in_months[temp_date_vector[2]],sep="-")
			} else
			{
				dates[j]="NA"
			}
		}
		
		raster_object_from_files@zname="Date/time"
		raster_object_from_files@zvalue=dates
	}
	
	if (raster_source=="narr")
	{
		# Robert Hijmans cleverly figured out how to auto-populate the dates... 
		if (input_files_N > 1)
		{
			raster_object_from_files=stack(sort(input_files))
		#	print("For NARR, there should only be one input file (a .nc file), please fix...")
		#	return()
		} else
		{
			raster_object_from_files=brick(sort(input_files))
		}
		projection(raster_object_from_files)="+proj=lcc +lat_1=50 +lat_2=50 +lat_0=50 +lon_0=-107 +x_0=5632642 +y_0=4612546"
	}
	
	if (raster_source=="generic")
	{
		raster_object_from_files=stack(sort(input_files))
		if(!missing(proj))
		{
			projection(raster_object_from_files)=proj
		}
		
		if(missing(zname))
		{
			raster_object_from_files@zname="NA"
		} else
		{
			raster_object_from_files@zname=zname
			if(zname=="months")
			{
				if (nlayers(raster_object_from_files)!=12)
					# || length(zvalue) != 12
				{
					print("zname=months requires exactly 12 layers in the files and zvalue to be of length 12...")
					return()
				}
				if(missing(zvalue))
				{
					raster_object_from_files@zvalue=as.character(1:12)
				} else
				{
					raster_object_from_files@zvalue=zvalue
				}
			}
			if(zname=="Date/time")
			{
				if(missing(zvalue))
				# || length(zvalue) != nlayers(raster_object_from_files)
				{
					print("zname=Date/time requires a zvalue vector the length of the number of input files")
					return()
				} else
				{
					# Should probably check to make sure its a good date/time string
					raster_object_from_files@zvalue=zvalue
				}
			}
		}
	
	}
	
	# The varname may be generalizable for NARR files, not sure yet.

	
	return(raster_object_from_files)
	
}
