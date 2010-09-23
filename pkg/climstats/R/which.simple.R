# TODO: Add comment
# 
# Author: jonathan
###############################################################################


which.max.simple=function(x,na.rm=TRUE)
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
		return(NA)
	} else
	{
		return(which.max(x))
	}
}

which.min.simple=function(x,na.rm=TRUE)
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
		return(NA)
	} else
	{
		return(which.min(x))
	}
}

