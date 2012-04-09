#' Create Raster from NCDF
#' 
#' Creates a raster from an NCDF file
#' 
#' 
#' @param ncdf_gaussian_grid_fname TODO
#' @param reference TODO
#' @author Jonathan A. Greenberg
#' @seealso TODO
#' @keywords format brick/stack
#' 
#' 
ncdf_gaussian_grid_to_raster=function(ncdf_gaussian_grid_fname,reference)
{
	ncopen=open.ncdf(ncdf_gaussian_grid_fname)
	v1 <- ncopen$var[[1]]
	lat<- get.var.ncdf(ncopen, varid = 'lat')
	lon<- get.var.ncdf(ncopen, varid = 'lon')
	time<- get.var.ncdf(ncopen, varid = 'time')
	data <- get.var.ncdf(ncopen,v1)
	close.ncdf(ncopen)
	
	x<- rep(lon, length(lat))
	y<- rep(lat, each = length(lon))
	sp<- SpatialPoints(cbind(x,y))
	projection(sp)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
	
	i=1
	temp_single_layer=as.vector(data[,,i])
	
#	interpolated_raster = pointsToRaster(reference,sp,values=temp_single_layer,fun=mean)
	# This has not been tested yet, but should work.
	interpolated_raster = rasterize(sp,reference,field=temp_single_layer,fun=mean)
	
}
