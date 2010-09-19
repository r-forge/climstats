# TODO: Add comment
# 
# Author: jonathan
###############################################################################


get_climate_data <- function(climate_source,dates,startdate,enddate,download_folder,final_folder,standardize=TRUE,snow_nthreads=1,overwrite=FALSE)
{	
	apply_gains_offsets=function(x,gains,offsets,divide_by_days_in_month=FALSE,snow_nthreads=1)
	{
		require("chron")
		require("raster")
		
		dates_to_days_in_month=function(date_vector)
		{
			require("chron")
			days_in_months=c(31,28,31,30,31,30,31,31,30,31,30,31)
			date_vector_chron=as.chron(as.POSIXct(date_vector,origin="1970-01-01"))
			dates_to_days_in_month=days_in_months[unclass(months(date_vector_chron))]
			dates_to_days_in_month[leap.year(date_vector_chron) & months(date_vector_chron)=="Feb"]<-29
			return(dates_to_days_in_month)
		}
		
		if(missing(gains))
		{
			gains=1
		}
		if(missing(offsets))
		{
			offsets=0
		}
		
		if((class(x)=="RasterStack") | (class(x)=="RasterBrick") | (class(x)=="RasterLayer"))
		{
			
			x_nlayers=nlayers(x)
			if(length(gains)==1)
			{
				gains=rep(gains,x_nlayers)
			}
			if(length(offsets)==1)
			{
				offsets=rep(offsets,x_nlayers)
			}
			if(length(gains)!=x_nlayers | length(offsets)!=x_nlayers)
			{
				print("gains and offsets must be of length 1 or length = nlayers(x)")
				return(NULL)
			}
			if(divide_by_days_in_month)
			{
				dates_to_days_in_month=dates_to_days_in_month(x@zvalue)
				gains=gains/dates_to_days_in_month
			}
			gains=as.list(gains)
			offsets=as.list(offsets)
			
			if(class(x)=="RasterStack" | class(x)=="RasterBrick")
			{
				x_list=brickstack_to_raster_list(x)
			} else
			{
				x_list=list(x)
			}
			
			if((snow_nthreads) > 1)
			{
				require("snow")
				cl <- makeCluster(snow_nthreads, type = "MPI") 
				x_list_gain_offset=clusterMap(cl,function(r,g,o) { r*g+o },r=x_list,g=gains,o=offsets)
				stopCluster(cl)
			} else
			{
				x_list_gain_offset=mapply(function(r,g,o) { r*g+o },r=x_list,g=gains,o=offsets)
			}
			if(x_nlayers>1)
			{
				x_gain_offset=stack(x_list_gain_offset)
			} else
			{
				x_gain_offset=x_list_gain_offset[[1]]
			}
			x_gain_offset@zvalue=x@zvalue
			
		} else
		{
			# Nothing yet
		}
		return(x_gain_offset)
		
	}
	

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
			dates=seq.Date(startdate,enddate,by="month")
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
		# Download and extract the files
		if(!missing(download_folder))
		{
			setwd(download_folder)
		}
		
		for(i in 1:dates_N)
		{
			download.file(prism_path[i],destfile=basename(prism_path[i]))
			
		}
		for(i in 1:dates_N)
		{
			gunzip(prism_filenames[i],remove=TRUE)
		}
		
		if(standardize)
		{
			require("raster")			
			# Set up each file as a raster.
			prism_filenames_gunzipped=paste(paste("us",prism_type,years_text,sep="_"),months_text,sep=".")
			raster_names_list=as.list(prism_filenames_gunzipped)
			raster_list=sapply(raster_names_list,raster,simplify=FALSE)
			
			# For PRISM ASCIIs, its faster if we pre-convert them before we tweak them any further.
			setOptions(setfileext=FALSE)
			for (i in (1:dates_N))
			{
				writeRaster(raster_list[[i]],paste(prism_filenames_gunzipped[i],"grd",sep="."),format="raster",overwrite=TRUE)	
			}
			setOptions(setfileext=TRUE)
			
			raster_list=sapply(paste(raster_names_list,"grd",sep="."),raster,simplify=FALSE)
			
			
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
			for (i in (1:dates_N))
			{
				raster_list[[i]]@zvalue=as.character(dates_raster[i])
			}
		
			if(prism_type=="ppt")
			{
				if((snow_nthreads) > 1)
				{
					require("snow")
					cl <- makeCluster(snow_nthreads, type = "MPI") 
					final_raster_list=clusterMap(cl,apply_gains_offsets,raster_list,MoreArgs=list(gains=(1/100),divide_by_days_in_month=TRUE))
					stopCluster(cl)
				} else
					final_raster_list=mapply(apply_gains_offsets,raster_list,MoreArgs=list(gains=(1/100),divide_by_days_in_month=TRUE))
				}
			}
			if(prism_type=="tmin" | prism_type=="tmax")
			{
				if((snow_nthreads) > 1)
				{
					require("snow")
					cl <- makeCluster(snow_nthreads, type = "MPI") 
					final_raster_list=clusterMap(cl,apply_gains_offsets,raster_list,MoreArgs=list(gains=(1/100),divide_by_days_in_month=FALSE))
					stopCluster(cl)
				} else
				{
					final_raster_list=mapply(apply_gains_offsets,raster_list,MoreArgs=list(gains=(1/100),divide_by_days_in_month=FALSE))
				}
				
		#		if(!missing(final_folder))
		#		{
		#			setwd(final_folder)
		#		}
				
				# Write out final files.
		#		setOptions(setfileext=FALSE)
		#		for (i in (1:dates_N))
		#		{
		#			writeRaster(final_raster_list[[i]],paste(final_folder,paste(prism_filenames_gunzipped[i],"grd",sep="."),sep="/"),format="raster",overwrite=TRUE)	
		#		}
		#		setOptions(setfileext=TRUE)
				
			}
			
		} # End PRISM
		
	
	return(final_raster_list)
	
}
