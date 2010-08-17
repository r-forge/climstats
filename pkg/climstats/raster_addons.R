# TODO: Add comment
# 
# Author: jonathan
###############################################################################


brickstack_to_raster_list=function(brickstack)
{
	if((class(brickstack)!="RasterStack") & (class(brickstack)!="RasterBrick"))
	{
		print("Input must be a RasterStack or RasterBrick")
		return()
	}
	brickstack_nlayers=nlayers(brickstack)
	brickstack_pos=1:brickstack_nlayers
#	raster_list=vector("list",brickstack_nlayers)
	
	raster_list=mapply(function(brickstack,layer) { raster(brickstack,layer=layer) },
			brickstack_pos,MoreArgs=list(brickstack=brickstack),SIMPLIFY=FALSE)
	
	return(raster_list)
}
