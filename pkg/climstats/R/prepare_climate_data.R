# TODO: Add comment
# 
# Author: jonathan
###############################################################################


# These functions are designed to convert widely available climate data to raster objects.

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
