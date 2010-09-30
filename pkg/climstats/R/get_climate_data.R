# TODO: Add comment
# 
# Author: jonathan
###############################################################################


get_climate_data <- function(
		climate_source,
		date_range,
		download_folder,
		final_folder,
		standardize=TRUE,
		snow_nthreads=1,
		overwrite=FALSE,
		wnd_speed_height_correction=TRUE,
		verbose=FALSE)
{	

	# climate_source:
	#	PRISM-800m-ppt: 	PRISM 800m monthly precipitation grids
	# 	PRISM-800m-tmin:	PRISM 800m monthly min temperature grids
	#	PRISM-800m-tmax:	PRISM 800m monthly max temperature grids
	#	PRISM-800m-spi:		PRISM 800m monthly standardized precipitation index grids
	#	PRISM-800m-tdmean:	PRISM 800m monthly dewpoint grids
	
	# dates: a vector of individual dates.  get_climate_data will download the product with the nearest matching date.
	# startdate/enddate: used in lieu of dates.
	
	# standardize=TRUE downloads the data, converts the units to climstats standards, and saves the
	#	data out as a raster or brick in the native raster format (grd).
	# stanardize=FALSE only downloads the data.
	

	
	if(missing(download_folder))
	{
		download_folder=getwd()
	}

	setwd(download_folder)
	
	if(missing(final_folder))
	{
		final_folder=getwd()
	}

	if(	climate_source=="PRISM-800m-ppt" |
		climate_source=="PRISM-800m-tmin" |
		climate_source=="PRISM-800m-tmax" |
		climate_source=="PRISM-800m-spi" |
		climate_source=="PRISM-800m-tdmean")
	{
		require("R.utils")
		
		
		basepath="ftp://prism.oregonstate.edu/pub/prism/us/grids"
		yearfolderstart=seq(1890,2010,by=10)
		yearfolderend=seq(1899,2019,by=10)
		yearfolders=paste(yearfolderstart,yearfolderend,sep="-")
		
		# PRISM is monthly data, so we figure the range based on months.
		if(missing(dates))
		{
			startdate=as.Date(date_range[1])
			enddate=as.Date(date_range[2])
			dates=seq.Date(startdate,enddate,by="month")
			if(verbose)
			{
				print("Downloading/preparing the following dates...")
				print(dates)
			}

		}
		
		years_text=format(dates,"%Y")
		months_text=format(dates,"%m")
		
		dates_N=length(dates)
		
		prism_type=strsplit(climate_source,"-")[[1]][3]
		prism_filenames=paste(paste("us",prism_type,years_text,sep="_"),months_text,"gz",sep=".")
		
		# Figure out the full path
		prism_path=vector(mode="character",length=dates_N)
		for (i in 1:dates_N)
		{
			temp_year=as.numeric(years_text[i])
			temp_yearfolderstart_check=yearfolderstart <= temp_year
			temp_yearfolderend_check=yearfolderend >= temp_year
			temp_yearfolder_check=temp_yearfolderstart_check & temp_yearfolderend_check
			temp_yearfolder=yearfolders[temp_yearfolder_check]
			
			prism_path[i]=paste(basepath,prism_type,temp_yearfolder,prism_filenames[i],sep="/")
			
		}
		
		prism_filenames_gunzipped=paste(paste("us",prism_type,years_text,sep="_"),months_text,sep=".")
		raster_names_list=as.list(prism_filenames_gunzipped)
		
		# Download and extract the files

		
		for(i in 1:dates_N)
		{
			if(
				# If overwrites are allowed...
				overwrite | 
				# If overwrites are disabled but the gunzipped or decompressed file is not present...
				(!overwrite & !file.exists(prism_filenames_gunzipped[[i]]) & !file.exists(prism_filenames[[i]]))
			)
			{
				download.file(prism_path[i],destfile=basename(prism_path[i]))
			}
		}
		for(i in 1:dates_N)
		{
			if(overwrite | (!overwrite & !file.exists(prism_filenames_gunzipped[[i]])))
			{
				gunzip(prism_filenames[i],remove=TRUE)
			}
		}
		
		if(standardize)
		{
			require("raster")			
			# Set up each file as a raster.
		#	prism_filenames_gunzipped=paste(paste("us",prism_type,years_text,sep="_"),months_text,sep=".")
		#	raster_names_list=as.list(prism_filenames_gunzipped)
		#	raster_list=sapply(raster_names_list,raster,simplify=FALSE)
			
			# For PRISM ASCIIs, its faster if we pre-convert them before we tweak them any further.
			setOptions(setfileext=FALSE)
			for (i in (1:dates_N))
			{
				if(overwrite |
						(!overwrite & !file.exists(paste(prism_filenames_gunzipped[i],"_raw.grd",sep="")))
				)
				{
					temp_raster=raster(raster_names_list[[i]])
					writeRaster(temp_raster,paste(prism_filenames_gunzipped[i],"_raw.grd",sep=""),format="raster",overwrite=TRUE)	
				}
			}
			setOptions(setfileext=TRUE)
			
#			raster_list=sapply(paste(raster_names_list,"_raw.grd",sep=""),raster,simplify=FALSE)
			
			raster_raw_list=as.list(paste(raster_names_list,"_raw.gri",sep=""))
#			stack_raw=stack(raster_raw_list)
			
			# Now assign the proper dates.
			days_in_months=c(31,28,31,30,31,30,31,31,30,31,30,31)
			middays_in_months=ceiling(days_in_months/2)
			dates_raster=vector("character",length=dates_N)
			for (i in (1:dates_N))
			{
				temp_date_vector=as.numeric(strsplit(strsplit(prism_filenames_gunzipped[i],"_")[[1]][3],"[.]")[[1]])
				if (temp_date_vector[2] >= 1 & temp_date_vector[2] <=12)
				{
					dates_raster[i]=paste(temp_date_vector[1],temp_date_vector[2],middays_in_months[temp_date_vector[2]],sep="-")
				} else
				{
					dates_raster[i]="NA"
				}
			}
			dates_raster=as.Date(dates_raster)
#			for (i in (1:dates_N))
#			{
#				raster_list[[i]]@zvalue=as.character(dates_raster[i])
#			}
		
			climstats_filenames=vector(mode="character",length=dates_N)
			for(i in (1:dates_N))
			{
				climstats_filenames[i]=paste(prism_filenames_gunzipped[i],"_climstats.grd",sep="")
			}

			if(prism_type=="ppt")
			{
#				if((snow_nthreads) > 1)
#				{
#					require("snow")
#					cl <- makeCluster(snow_nthreads, type = "MPI") 
#					final_raster_list=clusterMap(cl,apply_gains_offsets,raster_list,MoreArgs=list(gains=(1/100),divide_by_days_in_month=TRUE))
#					stopCluster(cl)
#				} else
#					final_raster_list=mapply(apply_gains_offsets,raster_list,MoreArgs=list(gains=(1/100),divide_by_days_in_month=TRUE))
#				}
	
				setOptions(setfileext=FALSE)
				for (i in (1:dates_N))
				{
					if(overwrite |
							(!overwrite & !file.exists(climstats_filenames[i]))
							)
					{
						setwd(download_folder)
						temp_raster=raster(raster_raw_list[[i]])
						temp_raster@zvalue=as.character(dates_raster[i])
						temp_raster@zname="Date/time"
						temp_raster_standardized=apply_gains_offsets(temp_raster,gains=(1/100),divide_by_days_in_month=TRUE)
						setwd(final_folder)
						writeRaster(temp_raster_standardized,climstats_filenames[i],format="raster",overwrite=TRUE)	
					}
				}
				setOptions(setfileext=TRUE)
			}
			
			if(prism_type=="tmin" | prism_type=="tmax")
			{
				setOptions(setfileext=FALSE)
				for (i in (1:dates_N))
				{
					if(overwrite |
							(!overwrite & !file.exists(paste(prism_filenames_gunzipped[i],"_climstats.grd",sep="")))
							)
					{
						setwd(download_folder)
						temp_raster=raster(raster_raw_list[[i]])
						temp_raster@zvalue=as.character(dates_raster[i])
						temp_raster@zname="Date/time"
						temp_raster_standardized=apply_gains_offsets(temp_raster,gains=(1/100),divide_by_days_in_month=FALSE)
						projection(temp_raster_standardized)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
						setwd(final_folder)
						writeRaster(temp_raster_standardized,climstats_filenames[i],format="raster",overwrite=TRUE)	
					}
				}
				setOptions(setfileext=TRUE)
			}	
			
			setwd(final_folder)
			climstats_stack=stack(as.list(climstats_filenames))
			climstats_stack@zvalue=as.character(dates_raster)
			climstats_stack@zname="Date/time"
			projection(climstats_stack)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
			return(climstats_stack)
			}
			
		} # End PRISM
		
		# North American Regional Reanalysis data (monthly mean)
		# http://www.esrl.noaa.gov/psd/data/gridded/data.narr.html
		if(climate_source=="NARR-monthlymean-wnd")
		{
			require("ncdf")
			basepath="ftp://ftp.cdc.noaa.gov/Datasets/NARR/Derived/monolevel"

	#		if(missing(dates))
	#		{
	#			dates=seq.Date(startdate,enddate,by="month")
	#		}
			
	#		dates_N=length(dates)
			
			if(!missing(download_folder))
			{
				setwd(download_folder)
			}
			
			# Set up filenames to download.
			if(climate_source=="NARR-monthlymean-wnd")
			{
				download_filenames=c("uwnd.10m.mon.ltm.nc","vwnd.10m.mon.ltm.nc")
				download_path=paste(basepath,download_filenames,sep="/")
			}	
			for(i in 1:length(download_filenames))
			{
				if(
						# If overwrites are allowed...
						overwrite | 
						# If overwrites are disabled but the file is not present...
						(!overwrite & !file.exists(download_filenames[i]))
						)
				{
					download.file(download_path[i],destfile=download_filenames[i])
				}
			}
			
			# Preprocess the data if requested.
			if(standardize)
			{
				narr_brick_list=vector(mode="list",length=length(download_filenames))
				for(i in 1:length(download_filenames))
				{
					narr_brick_list[[i]]=brick(download_filenames)
				}
				
				if(climate_source=="NARR-monthlymean-wnd")
				{				
					if(!wnd_speed_height_correction)
					{
						wnd=windvectors_to_wind(narr_brick_list[[1]],narr_brick_list[[2]])
					} else
					{
						wnd=windvectors_to_wind(narr_brick_list[[1]],narr_brick_list[[2]],10)
					}
					
					
					if(!missing(final_folder))
					{
						setwd(final_folder)
					}
					
					setOptions(setfileext=FALSE)
					projection(wnd)="+proj=lcc +lat_1=50 +lat_2=50 +lat_0=50 +lon_0=-107 +x_0=5632642 +y_0=4612546"
					writeRaster(wnd,"wnd.10m.mon.ltm.grd",format="raster",overwrite=TRUE)	
					setOptions(setfileext=TRUE)
					
					return(wnd)
				}
			}
			
		}
		
	
#	return(final_raster_list)
	
}
