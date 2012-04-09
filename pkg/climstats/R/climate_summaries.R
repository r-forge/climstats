#' Summarize Climate Data
#' 
#' Summarizes climate data over desired date ranges, across monthly, annual, or
#' full time intervals, and (optionally) across periods defined by other
#' climate variables.
#' 
#' 
#' @param climate_data A climate data raster to which summary_type is applied;
#' summaries of these values are returned except when the summary method is
#' which.min.simple or which.max.simple.
#' @param date_range The date range over which climate_data should be
#' summarized.
#' @param summary_type The type of summary applied to climate_data (options are
#' "which.min.simple", "which.max.simple",
#' "min","max","sum","mean","sd","cv","spi","quantile").
#' @param summary_interval The interval over which values are returned (options
#' are "all", "monthly", \cr "yearly")
#' @param apply_maxmin A climate data raster containing values to be summarized
#' and returned if \cr which.min.simple or which.max.simple is summary method.
#' @param verbose verbose=TRUE will print process information.
#' @param probs Numeric vector of probabilities with values in [0,1] used by
#' \cr summary_type=quantile.
#' @param tie_value Controls how non-unique extreme values are accommodated by
#' the summary method when summary_type is which.min.simple or
#' which.max.simple.
#' @return Returns a raster layer.
#' @author Jonathan A. Greenberg
# @seealso \code{\link[climstats]{get_climate_data}},
# \code{\link[climstats]{eto}}, \code{\link{which.min.simple}},
# \code{\link{which.max.simple}}
#' @keywords climate
#' @examples \dontrun{
#' 
#' # Use example file for Tahoe, California, USA
#' require(R.utils)
#' require(climstats)
#' 
#' load(system.file("extdata/pptTahoe.RData",package="climstats"))
#' # View dates of precipitation file
#' getZ(pptTahoe)
#' 
#' # View summary
#' #	(this layer was standardized; units are average mm H2O per day)
#' pptTahoe
#' 
#' ##Summarize precipitation
#' 
#' # Total precipitation for 1992
#' date_range_92=c("1992-01-01","1992-12-31")
#' ppt_92_perday=climate_summaries(climate_data=pptTahoe,
#' 	date_range=date_range_92,summary_type="mean",
#' 	summary_interval="yearly")
#' # Convert from average mm H2O/day to total mm H2O/day
#' ppt_92=ppt_92_perday*365
#' 
#' # View both 1992 and 1993 precipitation
#' date_range_9293=c("1992-01-01","1993-12-31")
#' ppt_9293_perday=climate_summaries(climate_data=pptTahoe,
#' 	date_range=date_range_9293,summary_type="mean",
#' 	summary_interval="yearly")
#' ppt_9293=ppt_9293_perday*365
#' ppt_9293
#' # Total precipitation was lower in 1992 than in 1993
#' 
#' #Now get average annual precipitation for the period 1992-1993
#' ppt_mean_perday=climate_summaries(climate_data=pptTahoe,
#' 	date_range=date_range_9293,summary_type="mean",summary_interval="all")
#' ppt_mean_all=ppt_mean_perday*365	
#' 
#' # Download and post-process PRISM monthly precipitation data
#' 		ppt_perday = get_climate_data("PRISM-4km-ppt", 
#' 			date_range = c("1999/1/1","2000/12/31"),standardize = TRUE, 
#' 			overwrite = FALSE,enable_download = T, verbose = T)
#' 		summary(ppt_perday)
#' 		getZ(ppt_perday)
#' 		#This file can be used for ETO or other calculations 
#' 		#	requiring daily averages 
#' 
#' 		ppt = get_climate_data("PRISM-4km-ppt", date_range = c("1999/1/1", 
#' 			"2000/12/31"),standardize = FALSE, overwrite = FALSE, 
#' 			enable_download = T, verbose = T)
#' 		summary(ppt)
#' 		#Convert PRISM data to total mm H2O/month
#' 		ppt=apply_gains_offsets(pptTahoe,gains=(1/100),
#' 			divide_by_days_in_month=FALSE)
#' 		#This file can be used to calculate total precipitation
#' 
#' # Annual Precipitation		
#' 		date_range=c("1999-01-01","2000-12-31")
#' 		ppt_month_mean=climate_summaries(climate_data=ppt,date_range=date_range,
#' 			summary_type="mean",summary_interval="all")
#' 		ppt_mean_all=12*ppt_month_mean	
#' 		#This is an alternative to using the "sum" function
#' 		
#' # Monthly Precipitation
#' 		ppt_mean_monthly=climate_summaries(ppt,date_range=date_range,
#' 			summary_type="mean",summary_interval="monthly")
#' 		
#' # Precipitation of Wettest Month
#' 		monthly_date_range=c("9999-01-01","9999-12-31")
#' 		ppt_of_max_ppt_mean_monthly=
#' 			climate_summaries(climate_data=ppt_mean_monthly,
#' 			date_range=monthly_date_range,summary_type="which.max.simple",
#' 			summary_interval="all",apply_maxmin=ppt_mean_monthly)
#' 		
#' # Precipitation of Driest Month			
#' 		ppt_of_min_ppt_mean_monthly=
#' 			climate_summaries(climate_data=ppt_mean_monthly,
#' 			date_range=monthly_date_range,summary_type="which.min.simple",
#' 			summary_interval="all",apply_maxmin=ppt_mean_monthly)
#' 		
#' # Precipitation Seasonality (Coefficient of Variation)
#' 		ppt_mean_monthly_cv_all=
#' 			climate_summaries(climate_data=ppt_mean_monthly,
#' 			date_range=monthly_date_range,summary_type="cv",summary_interval="all")
#' 	
#' # Min standard precipitation index (SPI)
#' 		ppt_spi_all=climate_summaries(climate_data=ppt,date_range=date_range,
#' 			summary_type="spi",summary_interval="all")
#' 		setZ(ppt_spi_all,getZ(ref),name='time')
#' 		ppt_spi_all_min=climate_summaries(climate_data=ppt_spi_all,
#' 			date_range=date_range,summary_type="min",summary_interval="all")
#' 		
#' # Max standard precipitation index (SPI)
#' 		ppt_spi_all_max=climate_summaries(climate_data=ppt_spi_all,
#' 			date_range=date_range,summary_type="max",summary_interval="all")
#' }
#' 
climate_summaries <- function(climate_data,date_range,summary_type,summary_interval="all",apply_maxmin,verbose=FALSE,
		probs,
		# For which.max/min.simple
		tie_value="random"
		)
{
	# This works on rasters only.
	
	climate_data_dates_idx=1:nlayers(climate_data)
	print(date_range)
	if(!missing(date_range))
	{
		climate_data_dates=as.Date(getZ(climate_data))
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
		climate_data_dates=as.Date(getZ(climate_data))
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
	if(summary_type=="sum")
	{
		climate_summary=stackApply(climate_data_subset, summary_interval_idx, sum)
	}
	
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
	#	print(climate_data_subset)
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
	
	setZ(climate_summary,zvalue_final)
	return(climate_summary)
}
