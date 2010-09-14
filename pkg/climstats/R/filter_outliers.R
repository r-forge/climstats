# TODO: Add comment
# 
# Author: jonathan
###############################################################################

find_outliers <- function(x,method="tukeys")
{
	if(method=="tukeys")
	{
		quantiles=quantile(x)[c(2,4)]
		iqr=as.numeric(diff(quantiles))
		outliers_vector=((x > quantiles[1] - 1.5*iqr) & (x < quantiles[1] + 1.5*iqr))
	}
	return(outliers_vector)
}

filter_outliers = function(x,method="tukeys")
{
	if(class(x)=="zoo")
	{
		outliers_vector=find_outliers(x,method)
		x_filtered=zoo(coredata(x)[outliers_vector],index(x)[outliers_vector])
	}
	if(class(x)=="spZoo" | class(x)=="station")
	{
		outliers_vector=find_outliers(x@zoo,method)
		x_filtered_zoo=zoo(coredata(x@zoo)[outliers_vector],index(x@zoo)[outliers_vector])
		x_filtered=x
		x_filtered@zoo=x_filtered_zoo
	}
	return(x_filtered)
}