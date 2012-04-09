#' Get/Preprocess Climate Data from Remote Sources
#' 
#' This function downloads and preprocesses climate data from a variety of
#' sources.
#' 
#' 
#' @param climate_source A datasource to fetch climate data from.  The
#' following datasources are currently supported: \itemize{ \item
#' "PRISM-4km-ppt": PRISM 4km monthly precipitation grids \item
#' "PRISM-4km-tmin": PRISM 4km monthly minimum temperature grids \item
#' "PRISM-4km-tmax": PRISM 4km monthly maximum temperature grids \item
#' "PRISM-4km-spi": PRISM 4km monthly standardized precipitation index grids
#' \item "PRISM-4km-tdmean": PRISM 4km monthly dewpoint grids \item
#' "PRISM-4km-elev": PRISM 4km elevation data \item "NARR-monthlymean-wnd":
#' North American Regional Reanalysis \cr monthly mean wind velocity \item
#' "NARR-longtermmonthlymean-wnd": North American Regional Reanalysis long-term
#' monthly mean wind velocity \item "generic": arbitrary local files.
#' local_files must be set if this is used. }
#' @param date_range A vector of length 2 containing the starting date and
#' ending date of a range of climate data to download.
#' @param dates Unused at present.
#' @param download_folder The target download directory of the raw data.
#' @param final_folder The final directory to store the preprocessed grd files.
#' @param standardize standardize=TRUE converts the downloaded dataset to
#' standard units used by eto and water balance \itemize{ \item precipitation:
#' average mm H20 per day \item temperature: deg C }
#' @param snow_nthreads The number of threads (CPUs) to use when processing.
#' If snow_nthreads>1, the algorithm will attempt to use the multiple
#' processing package snow via MPI.  CURRENTLY UNSUPPORTED.
#' @param overwrite overwrite=TRUE will allow overwriting of downloaded and
#' preprocessed files.  Setting overwrite=FALSE (default) lets the process pick
#' up where it left off if interrupted.
#' @param wnd_speed_height_correction wnd_speed_height_correction=TRUE
#' (default) corrects wind speed data to wind speed at 2m above the terrain
#' using the wind profile relationship described in
#' http://www.fao.org/docrep/x0490e/x0490e07.htm equation 47.
#' @param verbose verbose=TRUE will print process information.
#' @param enable_download If enable_download=TRUE (default), data will be
#' downloaded, otherwise download will be skipped.
#' @param local_files A search string for local files which will be used to
#' create the climate stack when climate_source="generic"
#' @param preconvert If preconvert=TRUE (default), raw data will be
#' preconverted to format=raster before any further processing occurs.
#' @param output_basename A string representing the output files.  Note that
#' most formats have their own defaults which will override this.
#' @param proj A PROJ string which will be used for climate_source='generic'.
#' @param zvalue The zvalue applied to the output raster (only used with
#' climate_source= 'generic'). This should be a character string of dates
#' for each of the output layers.  If zvalue='months' and the output is exactly
#' 12 bands, the zvalue will be set to dates corresponding to the first of each
#' month.
#' @return Returns a RasterLayer, RasterBrick or RasterStack object, as well as
#' saving the objects to disk.
#' @author Jonathan A. Greenberg
# @seealso \code{\link[climstats]{eto}},
# \code{\link[climstats]{climate_summaries}}
#' @references \itemize{ \item PRISM data:
#' \url{http://www.prism.oregonstate.edu/} \item PRISM terms of use:
#' \url{http://www.prism.oregonstate.edu/terms.phtml} \item PRISM metadata:
#' \url{http://www.prism.oregonstate.edu/docs/meta} } \itemize{ \item NCEP
#' North American Regional Reanalysis: \url{http://www.esrl.noaa.gov/psd/ \cr
#' data/gridded/data.narr.html} \item NCEP NARR Data Description:
#' \url{ftp://ftp.cdc.noaa.gov/Datasets/ \cr NARR/README} }
#' @keywords climate
#' @examples \dontrun{
#' 	# Download and decompress PRISM monthly precipitation data 
#' 	# but do not post-process it.
#' 	 ppt=get_climate_data("PRISM-4km-ppt", 
#' 		date_range=c("1999/1/1","1999/12/31"),
#' 		standardize=FALSE,	enable_download=TRUE)
#' 	# Note the raw, decompressed downloaded data.
#' 	 dir()
#' 	# Download and post-process PRISM monthly precipitation data 
#' 	# (note that as long as overwrite=FALSE, it will not redownload the files, 
#' 	# just post-process the files already acquired during the previous step.
#' 	 ppt=get_climate_data("PRISM-4km-ppt", 
#' 		date_range=c("1999/1/1","1999/12/31"),
#' 		standardize=TRUE, enable_download=TRUE, overwrite=TRUE)
#' 	# Note the raw raster format files (*_raw.grd), and the corrected raster 
#' 	# format data (*_climstats.grd).
#' 	 dir()
#' 	 summary(ppt)
#' 	 getZ(ppt)
#' 	}

get_climate_data <- function(
		climate_source,
		date_range,
		dates,
		download_folder,
		final_folder,
		standardize=TRUE,
		snow_nthreads=1,
		overwrite=FALSE,
		wnd_speed_height_correction=TRUE,
		verbose=FALSE,
		enable_download=FALSE,
		local_files,
		preconvert=TRUE,
		output_basename="climstats_data",
		proj,
		zvalue)
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
	# standardize=FALSE only downloads the data.
	

	
	if(missing(download_folder))
	{
		download_folder=getwd()
	}

	setwd(download_folder)
	
	if(missing(final_folder))
	{
		final_folder=getwd()
	}

	if(	climate_source=="PRISM-4km-elev")
	{
		require("R.utils")
		basepath="ftp://prism.oregonstate.edu/pub/prism/us/grids"
		prism_filenames="us_25m_dem.asc.gz"
		prism_filenames_gunzipped="us_25m_dem.asc"
		prism_path=paste(basepath,prism_filenames,sep="/")
	
		if(enable_download)
		{
			if(
					# If overwrites are allowed...
					overwrite | 
					# If overwrites are disabled but the gunzipped or decompressed file is not present...
					(!overwrite & !file.exists(prism_filenames_gunzipped) & !file.exists(prism_filenames))
					)
			{
				download.file(prism_path,destfile=basename(prism_path))
			}
		}
		
		if(overwrite | (!overwrite & !file.exists(prism_filenames_gunzipped)))
		{
			gunzip(prism_filenames,remove=TRUE,overwrite=TRUE)
		}
		
		if(standardize)
		{
			require("raster")	
			setOptions(setfileext=FALSE)
			if(overwrite |
				(!overwrite & !file.exists(paste(prism_filenames_gunzipped,"_climstats.grd",sep=""))))
			{
					temp_raster=raster(prism_filenames_gunzipped)
					writeRaster(temp_raster,paste(prism_filenames_gunzipped,"_climstats.grd",sep=""),format="raster",overwrite=TRUE)	
			}
			setOptions(setfileext=TRUE)
			elev=raster(paste(prism_filenames_gunzipped,"_climstats.grd",sep=""))
			projection(elev)="+proj=longlat +ellps=WGS84 +datum=WGS84 +lon_wrap"
			return(elev)
		}
			
	} # END PRISM-4km-elev
	
	
	if(	climate_source=="PRISM-4km-ppt" |
		climate_source=="PRISM-4km-tmin" |
		climate_source=="PRISM-4km-tmax" |
		climate_source=="PRISM-4km-spi" |
		climate_source=="PRISM-4km-tdmean")
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

		if(enable_download)
		{
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
			
		} # End PRISM monthly climate data
		
		# North American Regional Reanalysis data (monthly mean)
		# http://www.esrl.noaa.gov/psd/data/gridded/data.narr.html
		# This isn't working right now, don't use.
		if(climate_source=="NARR-monthlymean-rad_sw" | climate_source=="NARR-monthlymean-rad_lw")
		{
			require("ncdf")
			basepath="ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.derived/surface_gauss"

			if(climate_source=="NARR-monthlymean-rad_sw")
			{
				download_filenames="nswrs.mon.mean.nc"
				download_path=paste(basepath,download_filenames,sep="/")
			}
			
			if(climate_source=="NARR-monthlymean-rad_sw")
			{
				download_filenames="nlwrs.mon.mean.nc"
				download_path=paste(basepath,download_filenames,sep="/")
			}
			
			if(
					# If overwrites are allowed...
					overwrite | 
					# If overwrites are disabled but the file is not present...
					(!overwrite & !file.exists(download_filenames[i]))
					)
			{
				download.file(download_path[i],destfile=download_filenames[i])
			}
			
			if(standardize)
			{
				narr_brick_list=vector(mode="list",length=length(download_filenames))
				for(i in 1:length(download_filenames))
				{
					narr_brick_list[[i]]=brick(download_filenames)
				}
			}
			
		}



		# BEGIN NARR-longtermmonthlymean-wnd
		if(climate_source=="NARR-longtermmonthlymean-wnd")
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
			if(climate_source=="NARR-longtermmonthlymean-wnd")
			{
				download_filenames=c("uwnd.10m.mon.ltm.nc","vwnd.10m.mon.ltm.nc")
				download_path=paste(basepath,download_filenames,sep="/")
			}
			
			for(i in 1:length(download_filenames))
			{
				if(enable_download)
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
			}
			
			# Preprocess the data if requested.
			if(standardize)
			{
				narr_brick_list=vector(mode="list",length=length(download_filenames))
				for(i in 1:length(download_filenames))
				{
					narr_brick_list[[i]]=brick(download_filenames)
				}
				
				if(climate_source=="NARR-longtermmonthlymean-wnd")
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
			
		} # END NARR-longtermmonthlymean-wnd
		
		# BEGIN NARR-monthlymean-wnd
		if(climate_source=="NARR-monthlymean-wnd")
		{
			require("ncdf")
			basepath="ftp://ftp.cdc.noaa.gov/Datasets/NARR/Monthlies/monolevel"
			
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
				download_filenames=c("uwnd.10m.mon.mean.nc","vwnd.10m.mon.mean.nc")
				download_path=paste(basepath,download_filenames,sep="/")
			}
			
			for(i in 1:length(download_filenames))
			{
				if(enable_download)
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
			
		} # END NCEP-monthlymean-wnd
		
		# Needs passwords, boo.  Not pursuing this...
		if(climate_source=="PGF-daily-rad_sw" | climate_source=="PGF-daily-rad_lw")
		{
			
			if(climate_source=="PGF-daily-rad_sw")
			{
				basepath="http://dss.ucar.edu/datazone/dsszone/ds314.0/daily/"
				pgf_prefix="dlwrf_daily_"
			}
			#dlwrf_daily_1948-1948.nc
			if(missing(dates))
			{
				startdate=as.Date(date_range[1])
				enddate=as.Date(date_range[2])
				dates=seq.Date(startdate,enddate,by="year")
				if(verbose)
				{
					print("Downloading/preparing the following dates...")
					print(dates)
				}
			}
			years=format(dates,"%Y")
			pgf_filenames=paste(pgf_prefix,years,"-",years,".nc",sep="")
			dates_N=length(pgf_filenames)
			pgf_path=paste(basepath,pgf_filenames,sep="")
			
			for(i in 1:dates_N)
			{
				if(
						# If overwrites are allowed...
						overwrite | 
						# If overwrites are disabled but the gunzipped or decompressed file is not present...
						(!overwrite & !file.exists(pgf_filenames[[i]]))
						)
				{
					download.file(pgf_path[i],destfile=basename(pgf_path[i]))
				}
			}
		}
		
		if(climate_source=="generic")
		{
			# Search for the local files
			input_files=dir(dirname(local_files),
					pattern=basename(local_files),
					full.names=TRUE)
			input_files_N=length(input_files)
			
			if (input_files_N==0)
			{
				print("No files found, exiting")
				return()
			}
			# TODO: add preconversion here.
	
			raster_object_from_files=stack(sort(input_files))
			
			if(!missing(proj))
			{
				projection(raster_object_from_files)=proj
			}
			
			if(!missing(zvalue))
			{
			#	raster_object_from_files@zname=zname
				if(zvalue=="months")
				{
					if (nlayers(raster_object_from_files)!=12)
					# || length(zvalue) != 12
					{
						print("zvalue=months requires exactly 12 layers in the files and zvalue to be of length 12...")
						return()
					}
					raster_object_from_files@zvalue=as.character(seq.Date(as.Date("9999-01-01"),as.Date("9999-12-31"),by="month"))
				}else if(zvalue=="monthsyears"){
					if(missing(dates))
					{
						startdate=as.Date(date_range[1])
						enddate=as.Date(date_range[2])
						dates=seq.Date(startdate,enddate,by="month")
					}
					raster_object_from_files@zvalue=as.character(dates)
				}else
				{
					raster_object_from_files@zvalue=zvalue	
				}
			}
			return(raster_object_from_files)
		} # END generic
		
		
#	return(final_raster_list)
	
}
