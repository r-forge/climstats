# TODO: Add comment
# 
# Author: jonathan
###############################################################################


windvectors_to_wind=function(uwnd,vwnd,wnd_height)
{
	require(raster)
	# If wnd_height (in meters) is provided, data will be coerced to 2m elevation following:
	#   http://www.fao.org/docrep/x0490e/x0490e07.htm equation (47)
	# Please note this equation isn't exact -- using wnd_height=2 results in 
	#	a conversion factor of 1.000222, not 1.0.
	
	if(missing(uwnd) || missing(uwnd))
	{
		print("Missing one or both wind vectors, please try again...")
		return()
	}
	
	if(missing(wnd_height))
	{
		wnd_2m_conversion_factor=1
	} else
	{
		wnd_2m_conversion_factor=4.87/(log(67.8*wnd_height-5.42))
	}
	
	# 	Brick and stack math doesn't work yet, so let's use a workaround...	
	#	wnd=wnd_2m_conversion_factor*sqrt(uwnd^2+vwnd^2)

#	uwnd_list=brickstack_to_raster_list(uwnd)
#	vwnd_list=brickstack_to_raster_list(vwnd)
	
	wnd=((uwnd^2+vwnd^2)^(0.5))*wnd_2m_conversion_factor

	wnd@zvalue=uwnd@zvalue
	wnd@zname=uwnd@zname
	
	return(wnd)
}
