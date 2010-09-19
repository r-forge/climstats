# TODO: Add comment
# 
# Author: jonathan
###############################################################################

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
