# TODO: Add comment
# 
# Author: jonathan
###############################################################################


climate_summaries <- function(climate_data,date_range,summary_type,summary_interval="all",apply_maxmin)
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
	}
	
	if(summary_interval=="monthly")
	{
		summary_interval_idx=as.numeric(format(climate_data_dates_subset,"%m"))
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
	
	if(summary_type=="which.max.simple")
	{
		climate_summary=stackApply(climate_data_subset, summary_interval_idx, which.max.simple)
		if(!missing(apply_maxmin))
		{
			climate_summary_maxmin_mask=index_raster_to_mask(climate_summary)
			climate_summary=calc((climate_summary_maxmin_mask*apply_maxmin),sum)
		}
	}
	
	if(summary_type=="which.is.min")
	{
		climate_summary=stackApply(climate_data_subset, summary_interval_idx, which.min.simple)
	}
	return(climate_summary)
}
