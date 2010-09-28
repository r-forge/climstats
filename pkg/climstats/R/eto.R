# TODO: Add comment
# 
# Author: jonathan
###############################################################################


# TODO: Add comment
# 
# Author: jonathan
###############################################################################


eto_save <- function(tavg,tmin,tmax,rad,elev,wnd,G=0.0,a=0.23,model="pm",basename="eto_pm_climstats_",index_format="%Y-%m-%d",
		output_format="raster") 
{
#	if(!missing(tavg))
#	{
#		if(verbose)
#		{
#			print("Prepping tavg...")
#		}
#		tavg_list=brickstack_to_raster_list(tavg)
#	}
	if(verbose)
	{
		print("Prepping tmin...")
	}
	tmin_list=brickstack_to_raster_list(tmin)
	if(verbose)
	{
		print("Prepping tmax...")
	}
	tmax_list=brickstack_to_raster_list(tmax)
	if(verbose)
	{
		print("Prepping rad...")
	}
	rad_list=brickstack_to_raster_list(rad)
	if(verbose)
	{
		print("Prepping wnd...")
	}
	wnd_list=brickstack_to_raster_list(wnd)
	if(verbose)
	{
		print("Prepping tmin...")
	}
	dates_N=length(tmin_list)
	
	output_basenames=rep(basename,dates_N)
	output_dates=as.Date(tmin@zvalue,format=index_format)
	output_names=paste(output_basenames,output_dates,".grd",sep="")
	
	for(i in 1:dates_N)
	{
		if(verbose)
		{
			print(output_dates[i])
		}
#		if(!missing(tavg))
#		{
			temp_eto=eto(tmin=tmin_list[[i]],tmax=tmax_list[[i]],rad=rad_list[[i]],wnd=wnd_list[[i]],elev=elev,G=G,a=a,model=model)
#		} else
#		{
#			temp_eto=eto(tavg=tavg_list[[i]],tmin=tmin_list[[i]],tmax=tmax_list[[i]],rad=rad_list[[i]],wnd=wnd_list[[i]],elev=elev,G=G,a=a,model=model)
#		}
		setOptions(setfileext=FALSE)
		writeRaster(temp_eto,output_names[i],format=output_format,overwrite=TRUE)	
		setOptions(setfileext=TRUE)
	}
	

	
}
	
	
eto <- function(tavg,tmin,tmax,rad,elev,wnd,G=0.0,a=0.23,model="pm") 
{
	# From http://www.fao.org/docrep/x0490e/x0490e00.htm	
	
	# Should do some checks here (nlayers(input))
	
	# ETO inputs
	# tavg
	# tmin
	# tmax
	# rad: clear sky radiation
	# elev: meters a.s.l.
	# wnd: wind at 2m above surface
	# G: Soil flux (MJ/m2/day)
	# a: albedo
	
	if(model=="pm")
	{
		if(missing(tavg))
		{
			tavg=(tmax+tmin)/2
		}
		
		### Air humidity
		# eo=saturation vapour pressure at the air temperature T [kPa]
		eo_Tmax = .6108*exp(17.27*tmax/(tmax+237.3))
		eo_Tmin = .6108*exp(17.27*tmin/(tmin+237.3))
		
		# Mean saturation vapour pressure 
		es = (eo_Tmax + eo_Tmin)/2
		
		# Slope of saturation vapour pressure curve
		D = 4098*(.6108*exp((17.27*tavg)/(tavg+237.3)))/((tavg+237.3)^2)
		
		# Actual vapour pressure (ea) derived from dewpoint temperature with missing humidity data
		ea = (.6108*exp(17.27*(tmin-2)/((tmin-2)+237.3)))	#(48)
		
		Rns=rad*(1-a)
		Rnl=(4.903*10^(-9)*(((tmax+273.16)^(4)+(tmin+273.16)^(4))/2))*(0.34-0.14*sqrt(ea))*(1.35*1-0.35)
		
		Rn=Rns-Rnl
		
		g = 0.000665*101.3*(((293-.0065*elev)/293)^(5.26))
		eto_pm=((.408*D*(Rn-0)+
						g*(900/(tavg+273))*wnd*
						(es-ea))/(D+
						g*(1+0.34*wnd)))
		return(eto_pm)
	}
} # End ETO function
