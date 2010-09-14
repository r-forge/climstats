# TODO: Add comment
# 
# Author: jonathan
###############################################################################


add_leading_zeroes=function(number,number_length,max_number)
{
	if(!missing(max_number))
	{
		number_length=floor(log10(max_number))+1
	}
	fmt=paste("%0",number_length,"d",sep="")
	return(sprintf(fmt,number))
}
