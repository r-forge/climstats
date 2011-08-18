# TODO: Add comment
# 
# Author: jonathan
###############################################################################


# TODO: Add comment
# 
# Author: jonathan
###############################################################################

spatial_sync_vector <- function(unsynced,reference,verbose=FALSE)
{
	require("rgdal")
	new_projection=projection(reference)
	old_projection=projection(unsynced)

	if(new_projection!=old_projection)
	{
		synced_vector=spTransform(unsynced,CRS(new_projection))
	} else
	{
		synced_vector=unsynced
	}
	return(synced_vector)
}