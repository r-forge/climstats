# TODO: Add comment
# 
# Author: jonathan
###############################################################################


which.max.simple=function(x,na.rm=TRUE,tie_value="random")
{
	if(na.rm)
	{
		x=x[!is.na(x)]
	}
	if(length(x)==0)
	{
		return(NA)
	}
	maxval=max(x)
	if(is.na(maxval))
	{
		return(NA)
	}
	if(sum(x %in% maxval) > 1)
	{
		# Ties exist, figure out what to do with them.
		if(tie_value=="NA")
		{
			return(NA)
		}
		
		if(tie_value=="random")
		{
			tie_postions=which(x==maxval)
			return(sample(tie_postions,size=1))
		}
		
	} else
	{
		return(which.max(x))
	}
}

which.min.simple=function(x,na.rm=TRUE,tie_value="random")
{
	if(na.rm)
	{
		x=x[!is.na(x)]
	}
	if(length(x)==0)
	{
		return(NA)
	}
	minval=min(x)
	if(is.na(minval))
	{
		return(NA)
	}
	if(sum(x %in% minval) > 1)
	{
		# Ties exist, figure out what to do with them.
		if(tie_value=="NA")
		{
			return(NA)
		}
		
		if(tie_value=="random")
		{
			tie_postions=which(x==minval)
			return(sample(tie_postions,size=1))
		}
		
	} else
	{
		return(which.min(x))
	}
}

