# TODO: Add comment
# 
# Author: jonathan
###############################################################################


climate_summaries <- function(climate_data,date_range,summary_type,summary_interval="all",apply_maxmin,verbose=FALSE,
		probs,
		# For which.max/min.simple
		tie_value="random"
		)
{
	# This works on rasters only.
	
	climate_data_dates_idx=1:nlayers(climate_data)
	
	if(!missing(date_range))
	{
		climate_data_dates=as.Date(climate_data@zvalue)
		if(length(date_range)==2)
		{
			startdate=as.Date(date_range[1])
			enddate=as.Date(date_range[2])
			
			climate_data_dates_idx_subset=climate_data_dates_idx[(climate_data_dates >= startdate & climate_data_dates <= enddate)]
			climate_data_dates_subset=climate_data_dates[climate_data_dates_idx_subset]
			climate_data_subset=subset(climate_data,climate_data_dates_idx_subset)
		}
	} else
	{
		climate_data_dates_idx_subset=climate_data_dates_idx
		climate_data_dates_subset=climate_data_dates
		climate_data_subset=climate_data
	}	
	
	# Set up intervals
	if(summary_interval=="all")
	{
		summary_interval_idx=rep(1,length(climate_data_dates_idx_subset))
		zvalue_final="9999-01-01"
	}
	
	if(summary_interval=="monthly")
	{
		summary_interval_idx=as.factor(as.numeric(format(climate_data_dates_subset,"%m")))
		zvalue_final=as.character(as.Date(paste("9999",unique(as.numeric(format(climate_data_dates_subset,"%m"))),1,sep="-")))
		
		# TO DO, FIX ZVALUE
	}
	
	if(summary_interval=="yearly")
	{
		summary_interval_idx=as.factor(as.numeric(format(climate_data_dates_subset,"%Y")))

		zvalue_final=as.character(as.Date(paste(unique(as.numeric(format(climate_data_dates_subset,"%Y"))),1,1,sep="-")))
		if(verbose)
		{
			print(summary_interval_idx)
			print(zvalue_final)
		}
	}
	
	# Perform
	if(summary_type=="mean")
	{
		climate_summary=stackApply(climate_data_subset, summary_interval_idx, mean)
	}
	
	if(summary_type=="min")
	{
		climate_summary=stackApply(climate_data_subset, summary_interval_idx, min)
	}
	
	if(summary_type=="max")
	{
		climate_summary=stackApply(climate_data_subset, summary_interval_idx, max)
	}
	
	if(summary_type=="sd")
	{
		climate_summary=stackApply(climate_data_subset, summary_interval_idx, sd)
	}
	
	if(summary_type=="cv")
	{
		climate_summary=stackApply(climate_data_subset, summary_interval_idx, cv)
	}
	
	if(summary_type=="spi")
	{
	# Indices are not working right now, so we are going to ignore the indices
	#	climate_summary=stackApply(climate_data_subset, summary_interval_idx, spi)
		climate_summary=calc(climate_data_subset,fun=spi.matrix,na.rm=TRUE)
	}
	
	if(summary_type=="which.max.simple")
	{
		climate_summary=stackApply(climate_data_subset, summary_interval_idx, which.max.simple, tie_value=tie_value)
		if(!missing(apply_maxmin))
		{
			climate_summary_maxmin_mask=index_raster_to_mask(climate_summary,nlayers=nlayers(climate_data))
			climate_summary=calc((climate_summary_maxmin_mask*apply_maxmin),sum)
		}
	}
	
	if(summary_type=="which.min.simple")
	{
		climate_summary=stackApply(climate_data_subset, summary_interval_idx, which.min.simple, tie_value=tie_value)
		if(!missing(apply_maxmin))
		{
			climate_summary_maxmin_mask=index_raster_to_mask(climate_summary,nlayers=nlayers(climate_data))
			climate_summary=calc((climate_summary_maxmin_mask*apply_maxmin),sum)
		}
	}
	
	if(summary_type=="quantile")
	{
		# We should probably check to make sure the probs are within 0 to 1.
		quantile_function=function(x, na.rm){  probs = probs; quantile(x, probs =
							probs, na.rm=na.rm, type=8 ) }	
		climate_summary=stackApply(climate_data_subset, summary_interval_idx, quantile_function)
	}
	
	climate_summary@zvalue=zvalue_final
	return(climate_summary)
}
