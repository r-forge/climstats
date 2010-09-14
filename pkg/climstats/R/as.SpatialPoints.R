# TODO: Add comment
# 
# Author: jonathan
###############################################################################


as.SpatialPoints <- function(x)
{
	if(inherits(x,"spZoo"))
	{
		return(x@SpatialPoints)
	}
}

merge.SpatialPoints <- function(x,proj4string,create_index=TRUE)
{
	# Merges a list of spatialpoint objects
	if(missing(proj4string))
	{
		proj4string=projection(x[[1]])
	}
	
	x_projected=mapply(function(x,CRSobj) spTransform(x,CRSobj),x,MoreArgs=list(CRSobj=CRS(proj4string)))
	x_coordinates=sapply(x_projected,coordinates)
	if(create_index)
	{
		index=data.frame(index=(1:dim(x_coordinates)[2]))
		x_merged=SpatialPointsDataFrame(coords=t(x_coordinates),proj4string=CRS(proj4string),data=index)
	} else
	{
		x_merged=SpatialPoints(t(x_coordinates),CRS(proj4string))
	}
	return(x_merged)	
}