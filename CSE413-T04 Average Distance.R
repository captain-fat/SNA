avg_dist <- function(x)
{
	if (!is.matrix(x))
	{
		warning("Argument is not a matrix: returning NA")
		return(NA_real_)
	}
  
	vector <- c()
	for (i in 1:nrow(x))
	{
		sum <- 0
		for (j in 1:ncol(x))
		{
			sum <- sum + as.integer(x[i,j])
		}
		avg <- sum/(nrow(x)-1)
		vector <- c(vector, avg)
	}
	return(vector)
}