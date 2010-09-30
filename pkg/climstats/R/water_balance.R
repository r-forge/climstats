# TODO: Add comment
# 
# Author: jonathan
###############################################################################


water_balance_save <- function(ppt,eto,model="stephenson",basename="water_balance_climstats_",index_format="%Y-%m-%d",
		output_format="raster",verbose=FALSE,overwrite=FALSE)
{
	if(verbose)
	{
		print("Prepping ppt...")
	}
	ppt_list=brickstack_to_raster_list(ppt)
	if(verbose)
	{
		print("Prepping eto...")
	}
	eto_list=brickstack_to_raster_list(eto)
	
	
	dates_N=length(ppt_list)
	
	output_basenames=rep(basename,dates_N)
	output_dates=as.Date(ppt@zvalue,format=index_format)
	
	if(model=="stephenson")
	{		
		output_names_wdf=paste(output_basenames,"wdf_",output_dates,".grd",sep="")
		output_names_aet=paste(output_basenames,"aet_",output_dates,".grd",sep="")
		
#		output_names_wdf=paste(output_basenames,"wdf_",output_dates,sep="")
#		output_names_aet=paste(output_basenames,"aet_",output_dates,sep="")
		
		for(i in 1:dates_N)
		{
			if(verbose)
			{
				print(output_dates[i])
			}
			
			if(overwrite | (!overwrite & (!file.exists(output_names_wdf[i]) | !file.exists(output_names_aet[i]))))
			{
				temp_eto_ppt_diff=eto_list[[i]]-ppt_list[[i]]
				temp_wdf=temp_eto_ppt_diff
				temp_wdf[temp_eto_ppt_diff < 0]=0
				temp_surplus_idx=temp_eto_ppt_diff < 0
				temp_deficit_idx=temp_eto_ppt_diff > 0
				temp_aet=(ppt_list[[i]]*temp_deficit_idx)+(eto_list[[i]]*temp_surplus_idx)
				setOptions(setfileext=FALSE)
				writeRaster(temp_wdf,output_names_wdf[i],format=output_format,overwrite=TRUE)	
				writeRaster(temp_aet,output_names_aet[i],format=output_format,overwrite=TRUE)
				setOptions(setfileext=TRUE)
			}
		}
		wdf_stack=stack(output_names_wdf)
		aet_stack=stack(output_names_aet)
		wdf_stack@zvalue=ppt@zvalue
		aet_stack@zvalue=ppt@zvalue
		water_balance_list=list(wdf_stack,aet_stack)
		return(water_balance_list)
		
		
	}
}
