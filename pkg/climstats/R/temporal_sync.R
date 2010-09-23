# TODO: Add comment
# 
# Author: jonathan
###############################################################################


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
		unsynced_dates=as.Date(unsynced_stack@zvalue)
	} else
	{
		unsynced_dates=as.Date(unsynced_dates)
	}
	
	if(missing(reference_dates))
	{
		reference_dates=as.Date(reference@zvalue)
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
		
		
		if(class(unsynced)=="character")
		{
			synced_fname_list=vector(mode="list",length=length(reference_months))
			synced_bands_list=vector(mode="list",length=length(reference_months))
			for(i in 1:length(reference_months))
			{
				synced_fname_list[[i]]=unsynced
				synced_bands_list[[i]]=reference_unsynced_idx[i]
			}
			synced_stack=stack(synced_fname_list,bands=synced_bands_list)
			synced_stack@zvalue=reference@zvalue
			
			return(synced_stack)
		}	
	}
}
