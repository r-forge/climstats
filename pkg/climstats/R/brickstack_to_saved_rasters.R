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

brickstack_to_saved_rasters = function(brickstack,output_basename,use_layernames=FALSE)
{
	brickstack_to_saved_raster=function(rasterlayer,output_filename,extension)
	{
		if(missing(extension))
		{
			print(output_filename)
			writeRaster(rasterlayer,output_filename,format="raster")
		}
		else
		{
			print(paste(output_filename,extension,sep=""))
			writeRaster(rasterlayer,paste(output_filename,extension,sep=""),format="raster")
		}
	}
	
	raster_list=brickstack_to_raster_list(brickstack)
	if (use_layernames)
	{
		setOptions(setfileext=FALSE)
		output_filenames_fullpath=lapply(raster_list,filename)
		output_filenames=lapply(output_filenames_fullpath,basename)
		mapply(brickstack_to_saved_raster,raster_list,output_filenames,extension=".grd")
	} else
	{
		setOptions(setfileext=TRUE)
		brickstack_nlayers=nlayers(brickstack)
		brickstack_pos=c(1:brickstack_nlayers)
		brickstack_pos_id=add_leading_zeroes(number=brickstack_pos,max_number=max(brickstack_pos))
		output_filenames=paste(output_basename,brickstack_pos_id,sep="")
		mapply(brickstack_to_saved_raster,raster_list,output_filenames)
	}
}
