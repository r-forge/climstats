# TODO: Add comment
# 
# Author: jonathan
###############################################################################

spatial_sync_raster <- function(unsynced,reference,method="ngb",verbose=FALSE)
{
	new_projection=projection(reference)
	old_projection=projection(unsynced)
	
	new_res=res(reference)
	old_res=res(unsynced)
	
	if(new_projection!=old_projection | new_res[1] != old_res[1] | new_res[2] != old_res[2])
	{
		pr_extent=projectExtent(unsynced, new_projection)
		# We need to fix the extent
		pr_extent <- setExtent(pr_extent,extent(reference))
		res(pr_extent)=res(reference)
		if(new_projection!=old_projection)
		{
			pr <- projectRaster(unsynced, pr_extent,method=method)
		} else
		{
			pr <- resample(unsynced, pr_extent,method=method)
		}
	}
	
	expanded_raster=expand(pr,reference)
	synced_raster=crop(expanded_raster,reference)


	# This in theory shouldn't be neccessary...
	extent(synced_raster)=extent(reference)
	
	return(synced_raster)
	
}