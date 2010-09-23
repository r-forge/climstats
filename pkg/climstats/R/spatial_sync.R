# TODO: Add comment
# 
# Author: jonathan
###############################################################################

spatial_sync_raster <- function(unsynced,reference,verbose=FALSE)
{
	new_projection=projection(reference)
	old_projection=projection(unsynced)
	
	new_res=res(reference)
	old_res=res(unsynced)
	
	if(new_projection!=old_projection | new_res[1] != old_res[1] | new_res[2] != old_res[2])
	{
		pr_extent=projectExtent(unsynced, new_projection)
		res(pr_extent)=res(reference)
		pr <- projectRaster(unsynced, pr_extent)
	}
	
	return(crop(expand(pr,reference),reference))
	
}