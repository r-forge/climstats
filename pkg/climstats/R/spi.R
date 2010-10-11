# TODO: Add comment
# 
# Author: jonathan
###############################################################################


spi <- function(ppt)
{
	require("lmom")
	# Compute the sample L-moments of the time series and then computes
	# 	the parameters of the PDF based on these.
	ppt_pe3_params=pelpe3(samlmu(ppt))
	# Compute the CDF of this PDF for each measured sample in the time series. 
	ppt_pe3_cdf=cdfpe3(ppt,pelpe3(samlmu(ppt)))
	# We have to fix the 0s following http://ccc.atmos.colostate.edu/pub/spi.pdf page 4
	q=sum(ppt_pe3_cdf==0)/length(ppt_pe3_cdf)
	ppt_pe3_cdf=q+((1-q)*ppt_pe3_cdf)
	# Compute the quantile function for the CDF (which is the number of stdevs from the mean)
	spi=qnorm(ppt_pe3_cdf)
	#	print(length(spi))
	return(spi)
}
