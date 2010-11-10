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
		res(pr_extent)=res(reference)
		pr <- projectRaster(unsynced, pr_extent,method=method)
	}
	
	synced_raster=crop(expand(pr,reference),reference)
	# This in theory shouldn't be neccessary...
	extent(synced_raster)=extent(reference)
	
	return(synced_raster)
	
}