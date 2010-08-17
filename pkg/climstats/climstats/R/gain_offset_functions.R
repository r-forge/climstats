# TODO: Add comment
# 
# Author: jonathan
###############################################################################

divide_by_days_in_months=function(x,dates)
{
	dates_to_days_in_month=function(date_vector)
	{
		require("chron")
		days_in_months=c(31,28,31,30,31,30,31,31,30,31,30,31)
		date_vector_chron=as.chron(as.POSIXct(date_vector,origin="1970-01-01"))
		dates_to_days_in_month=days_in_months[unclass(months(date_vector_chron))]
		dates_to_days_in_month[leap.year(date_vector_chron) & months(date_vector_chron)=="Feb"]<-29
		return(dates_to_days_in_month)
	}
	
	if((class(x)=="RasterStack") | (class(x)=="RasterBrick") | (class(x)=="RasterLayer"))
	{
		if(missing(dates))
		{
			dates=x@zvalue
		}
		days_in_months=dates_to_days_in_month(dates)
		x_divided_by_days_in_months=apply_gains_offsets(x,gains=(1/days_in_months))
		
	} else
	{
		if(missing(dates))
		{
			print("Missing dates...")
			return()
		}
	}
}

apply_gains_offsets=function(x,gains,offsets)
{
	if((class(x)=="RasterStack") | (class(x)=="RasterBrick") | (class(x)=="RasterLayer"))
	{
		x_nlayers=nlayers(x)
		
		if(missing(gains))
		{
			gains=rep(1,x_nlayers)
		}
		if(missing(offsets))
		{
			offsets=rep(0,x_nlayers)
		}
		
		x_list=brickstack_to_raster_list(x)
		
		x_gains_offsets=stack(mapply(function(x,gains,offsets)
			{ x*gains+offsets },x_list,gains,offsets,SIMPLIFY=FALSE))

		x_gains_offsets@zvalue=x@zvalue
		x_gains_offsets@zname=x@zname
		
	} else
	{
		x_gains_offsets=x*gains+offsets
	}
	

	
	return(x_gains_offsets)
}
