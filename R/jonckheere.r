#' @name jonckheere
#' @title Jonckheere
#' @param x input data frame
#' @keywords internal
#' @export

## Hardcoding for column order should be removed from these.

jonckheere <- function(x)
{
	outputmatrix <- matrix(nrow=ncol(x)-1, ncol=2)
	outputmatrix[1,1] <- "Response Metric"
	outputmatrix[1,2] <- "Jonckheere Test P-value"

	iterations <- ncol(x)-2
	for (i in 1:iterations)
	{
	lm <- lm(x[,i+2] ~ x[,2], data=x)
 	outputmatrix[i+1,1] <- colnames(x[i+2])
 	outputmatrix[i+1,2] <- cor.test(x[,i+2], x[,1], method="k")$p.value
	}
	return(outputmatrix)
}