# TODO: Add comment
# 
# Author: jonathan
###############################################################################


list_to_data.frame <- function(x)
{
	list_N=length(x)
	for (i in 1:list_N)
	{
		if(i==1)
		{
			x_data.frame=x[[1]]
		} else
		{
			x_data.frame=rbind(x_data.frame,x[[i]])
		}
	}
	return(x_data.frame)
}
