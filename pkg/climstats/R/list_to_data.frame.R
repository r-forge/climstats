#' Convert a List to a Data Frame
#' 
#' Takes a list of equal-sized objects and returns a data frame.
#' 
#' 
#' @param x a list of vectors of equal length
#' @return A data frame with listed vectors as rows.
#' @author Jonathan A. Greenberg, Alison R. Mynsberge
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @keywords format
#' @examples
#' 
#' x<-c(4,1:3,NA,4)
#' y<-c(2:4,1,1,NA)
#' length(x)==length(y)
#' list_to_data.frame(list(x,y))
#' 
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
