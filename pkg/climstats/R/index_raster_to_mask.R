#' Internal function
#' 
#' Internal function
#' 
#' 
#' @param index_raster TODO
#' @param nlayers TODO
#' @author Jonathan A. Greenberg
#' @seealso \code{\link[base]{which.min}}, \code{\link[base]{which.max}}
#' @keywords format brick/stack
#' 
#' 
index_raster_to_mask <- function(index_raster,nlayers)
{
	nlayers_list=as.list(1:nlayers)
	index_mask_list=stack(mapply(function(layer,index_raster) index_raster==layer,nlayers_list,MoreArgs=list(index_raster=index_raster)))
}
