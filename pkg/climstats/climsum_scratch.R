# TODO: Add comment
# 
# Author: jonathan
###############################################################################

# require(raster,chron)

package.skeleton(name="climstats",code_files=c(
				"/Users/jonathan/Documents/code/eclipse/climstats/climate_to_raster.R",
				"/Users/jonathan/Documents/code/eclipse/climstats/gain_offset_functions.R",
				"/Users/jonathan/Documents/code/eclipse/climstats/raster_addons.R",
				"/Users/jonathan/Documents/code/eclipse/climstats/windvectors_to_wind.R"),
		path="/Users/jonathan/Documents/code/eclipse/climstats/")


raster_tmpdir="/raid0/tmp/jonathan.greenberg/R_raster_tmp/"
setOptions(tmpdir=raster_tmpdir)

dem=climate_to_raster('/home/CFC/jonathan.greenberg/shares/RAID5/projects/us_climate/data/raster/dem/^us_25m_dem.asc$','prism_dem')
ppt=climate_to_raster('/home/CFC/jonathan.greenberg/shares/RAID5/projects/us_climate/data/raster/prism/ppt/^us_ppt_','prism_800m')
tmn=climate_to_raster('/home/CFC/jonathan.greenberg/shares/RAID5/projects/us_climate/data/raster/prism/tmin/^us_tmin_','prism_800m')
tmx=climate_to_raster('/home/CFC/jonathan.greenberg/shares/RAID5/projects/us_climate/data/raster/prism/tmax/^us_tmax_','prism_800m')
rad=climate_to_raster('/home/CFC/jonathan.greenberg/shares/RAID5/projects/us_climate/data/raster/radiation/.envi$','generic',zname='months')
uwnd=climate_to_raster('/home/CFC/jonathan.greenberg/shares/RAID5/projects/us_climate/data/raster/wind/^uwnd.10m.mon.mean.nc$','narr')
vwnd=climate_to_raster('/home/CFC/jonathan.greenberg/shares/RAID5/projects/us_climate/data/raster/wind/^vwnd.10m.mon.mean.nc$','narr')


wnd=windvectors_to_wind(uwnd,vwnd,10)
writeRaster(wnd,filename='wnd_2m_mon_mean',format='raster',overwrite=TRUE)

# wind: m/s @ 2m
# ppt: mm H20/day


ppt_mmH20_per_day=apply_gains_offsets(divide_by_days_in_months(ppt),gains=rep(1/100,nlayers(ppt)))

tmn_deg_C=apply_gains_offsets(tmn,gains=rep(1/100,nlayers(tmn)))
tmx_deg_C=apply_gains_offsets(tmx,gains=rep(1/100,nlayers(tmx)))
rad_MJ_m2_day=apply_gains_offsets(rad,gains=rep(0.0036,nlayers(rad)))
writeRaster(rad_MJ_m2_day,filename='rsun_monthly_rad_MJ_m2_day',format='raster',overwrite=TRUE)


