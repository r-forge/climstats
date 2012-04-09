#' Temporally Sync Rasters
#' 
#' Aligns ("syncs") a Raster to a reference Raster.
#' 
#' Matches the temporal resolution and extent of the unsynced raster object to
#' the reference raster object.  Files with 12 layers (1/month) are expanded to
#' multiple years using "by.month". Files with unique entries per month per
#' year are reduced to the temporal range of the reference object using
#' "by.year.month".
#' 
#' @param unsynced A raster object or a string pointing to a raster on disk to
#' be temporally synced to a reference file.
#' @param reference A raster object with reference zvalue dates specific to the
#' month and year.
#' @param synctype Type of temporal sync; options are "by.month" or
#' "by.year.month".
#' @param unsynced_dates Required if getZ(unsynced) does not provide dates for
#' unsynced raster.
#' @param reference_dates Required if getZ(reference) does not provide
#' reference dates for reference raster.
#' @param verbose verbose=TRUE will print process information.
#' @return Returns a RasterLayer, RasterBrick or RasterStack object.
#' @author Jonathan A. Greenberg
#' @seealso \code{\link[climstats]{spatial_sync_raster}}
#' @keywords calculate brick/stack
#' @examples
#' 
#' \dontrun{
#' require(climstats)
#' 
#' load(system.file("extdata/pptTahoe.RData",package="climstats"))
#' getZ(pptTahoe)
#' 
#' #Download and standardize 2 large monthly wind vector files
#' wnd=get_climate_data("NARR-monthlymean-wnd",
#' 		date_range=c("1992-01-01","1993-12-31"),
#' 		standardize=TRUE,enable_download=TRUE,verbose=TRUE)
#' getZ(wnd)
#' 
#' wnd_synced=temporal_sync_raster(wnd,pptTahoe,synctype="by.year.month",
#' 	verbose=TRUE)
#' getZ(wnd_synced)
#' }
#' 
temporal_sync_raster <- function(unsynced,reference,synctype="by.month",unsynced_dates,reference_dates,verbose=FALSE)
{
	# TODO: Check to make sure zvalues are assigned
	if(class(unsynced)=="character")
	{
		unsynced_stack=stack(unsynced)
	} else
	{
		unsynced_stack=unsynced
	}
	
	if(missing(unsynced_dates))
	{
		unsynced_dates=as.Date(getZ(unsynced_stack))
	} else
	{
		unsynced_dates=as.Date(unsynced_dates)
	}
	
	if(missing(reference_dates))
	{
		reference_dates=as.Date(getZ(reference))
	} else
	{
		reference_dates=as.Date(reference_dates)
	}
	
	
	if(synctype=="by.month")
	{
		if(verbose)
		{
			print("Syncing by month...")
		}
		unsynced_months=format(unsynced_dates,"%m")
		reference_months=format(reference_dates,"%m")
		
		unsynced_months_unique=unique(unsynced_months)
		if(length(unsynced_months_unique) != length(unsynced_months))
		{
			print('synctype="by.month" requires a single entry per month in the unsynced file')
			return()
		}
		
		synccheck=unique(reference_months %in% unsynced_months)
		if(length(synccheck) > 1 | synccheck[1]==FALSE)
		{
			print("Reference contains unsyncable dates, does unsynced file have 12 layers?")
			return()
		}
		
		unsynced_idx=1:(length(unsynced_months))
		reference_unsynced_idx=vector(mode="numeric",length=length(reference_months))
		for(i in 1:length(reference_months))
		{
			reference_unsynced_idx[i]=unsynced_idx[unsynced_months %in% reference_months[i]]		
		}
		
		
		# If unsynced is a raster* object
			unsynced_stack_list=brickstack_to_raster_list(unsynced_stack)
			synced_stack=stack(unsynced_stack_list[reference_unsynced_idx])
			setZ(synced_stack,getZ(reference))
			return(synced_stack)
	}
	
	if(synctype=="by.year.month")
	{
		if(verbose)
		{
			print("Syncing by year & month...")
		}
		unsynced_mo_yr=format(unsynced_dates,"%Y-%m")
		reference_mo_yr=format(reference_dates,"%Y-%m")
		
		unsynced_mo_yr_unique=unique(unsynced_mo_yr)
		if(length(unsynced_mo_yr_unique) != length(unsynced_mo_yr))
		{
			print('synctype="by.year.month" requires a single entry per month per year in the unsynced file')
			return()
		}
		
		synccheck=unique(reference_mo_yr %in% unsynced_mo_yr)
		if(length(synccheck) > 1 | synccheck[1]==FALSE)
		{
			print("Reference contains unsyncable dates, does unsynced file include all reference months and years?")
			return()
		}
		
		unsynced_idx=1:(length(unsynced_mo_yr))
		reference_unsynced_idx=vector(mode="numeric",length=length(reference_mo_yr))
		for(i in 1:length(reference_mo_yr))
		{
			reference_unsynced_idx[i]=unsynced_idx[unsynced_mo_yr %in% reference_mo_yr[i]]		
		}
		
		# If unsynced is a raster* object
			unsynced_stack_list=brickstack_to_raster_list(unsynced_stack)
			synced_stack=stack(unsynced_stack_list[reference_unsynced_idx])
			setZ(synced_stack,getZ(reference))
			return(synced_stack)
		
	}
}
