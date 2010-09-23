# TODO: Add comment
# 
# Author: jonathan
###############################################################################


# TODO: Add comment
# 
# Author: jonathan
###############################################################################


eto <- function(tavg,tmin,tmax,rad,elev,wnd,G=0.0,a=0.23,model="pm") 
{
	# From http://www.fao.org/docrep/x0490e/x0490e00.htm	
	
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
		
		Rns=(1-a)*rad
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
