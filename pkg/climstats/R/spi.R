spi.matrix <- function(ppt,na.rm=TRUE)
{
#	print(ppt)
#	print(dim(ppt))
	if(class(ppt)=="matrix")
	{
		spi_output=apply(ppt,2,spi)
	} else
	{
		spi_output=spi(ppt,na.rm=na.rm)
	}
	return(spi_output)
}



#' Calculates the Standardized Precipitation Index (SPI)
#' 
#' Calculates the standardized precipitation index (SPI).
#' 
#' 
#' @aliases spi.matrix spi
#' @param ppt A time series of precipitation amounts in a vector or a matrix
#' with precipitation time series in each column.
#' @param na.rm a logical indicating whether missing values should be removed.
#' @return \item{spi}{returns the SPI value for a precipitation vector.}
#' \item{spi.matrix}{returns a vector of standardized precipitation indices for
#' a precipitation matrix or the SPI value for a precipitation vector.}
#' @author Jonathan A. Greenberg, Alison R. Mynsberge
#' @keywords climate
spi <- function(ppt,na.rm=TRUE)
{
#	print(na.rm)
	require("lmom")
	ppt_N=length(ppt)
#	print(ppt_N)
	if(na.rm)
	{
		ppt=na.omit(ppt)
		if(length(ppt)==0)
		{
			return(rep(NA,ppt_N))
		}
	}
	# Compute the sample L-moments of the time series and then computes
	# 	the parameters of the PDF based on these.
	ppt_pe3_samlmu=samlmu(ppt)
#	print(ppt_pe3_samlmu)
	ppt_pe3_params=pelpe3(ppt_pe3_samlmu)
	# Compute the CDF of this PDF for each measured sample in the time series. 
	ppt_pe3_cdf=cdfpe3(ppt,ppt_pe3_params)
	# We have to fix the 0s following http://ccc.atmos.colostate.edu/pub/spi.pdf page 4
	q=sum(ppt_pe3_cdf==0)/length(ppt_pe3_cdf)
	ppt_pe3_cdf=q+((1-q)*ppt_pe3_cdf)
	# Compute the quantile function for the CDF (which is the number of stdevs from the mean)
	spi=qnorm(ppt_pe3_cdf)
	#	print(length(spi))
	return(spi)
}

# spi_output=mapply(spi,as.list(testpelp),MoreArgs=list(na.rm=TRUE))
# spi_output=apply(testpelp,2,spi)
