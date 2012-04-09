#' Create a List of Rasters
#' 
#' Converts a raster stack or a brick into a list of individual RasterLayers.
#' 
#' 
#' @param brickstack A raster brick or raster stack object.
#' @author Jonathan A. Greenberg, Alison R. Mynsberge
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' \code{\link[raster]{stack}}, \code{\link[raster]{brick}}
#' @keywords brick/stack
#' @examples \dontrun{
#' 
#' r <- raster(nrows=10,ncols=10)
#' r <- setValues(r,1:ncell(r))
#' #Stack
#' temp_stack <- stack(r, r/2, r*10)
#' brickstack_to_raster_list(temp_stack)
#' #Brick
#' temp_brick <- brick(r, r/2, r*10)
#' brickstack_to_raster_list(temp_brick)
#' }
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



#' Save a RasterStack or RasterBrick
#' 
#' Saves a raster stack or a brick as individual RasterLayers.
#' 
#' 
#' @param brickstack A raster brick or raster stack object.
#' @param output_basename A character string for naming the objects.
#' @param use_layernames If TRUE, filenames (from disk) are appended to
#' output_basename to designate individual layers. If FALSE, layers are
#' distinguished on disk based on their order in the RasterBrick or
#' RasterStack.
#' @return Saves rasters to the working directory.
#' @author Jonathan A. Greenberg
#' @seealso \code{\link[raster]{stack}}, \code{\link[raster]{brick}}
#' @keywords brick/stack
#' @examples
#' 
#' \dontrun{r <- raster(nrows=10,ncols=10)
#' r <- setValues(r,1:ncell(r))
#' #Stack
#' temp_stack <- stack(r, r/2, r*10)
#' brickstack_to_saved_rasters(temp_brick,output_basename="temp")
#' #Brick
#' temp_brick <- brick(r, r/2, r*10)
#' brickstack_to_saved_rasters(temp_brick,output_basename="bricklayers")}
#' 
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
