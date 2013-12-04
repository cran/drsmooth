#' @name dunnettst3
#' @title Dunnett's T3 Test
#' @param targetcolumn  Character string, name of response column to be tested
#' @param alpha  Significance level (numeric) to be used
#' @param direction  Direction of the anticipated difference
#' @param data  Input dataframe.
#' @keywords internal
#' @export

dunnettst3 <- function (targetcolumn, alpha, direction, data) {
	dunnett_t3 <- DTK.test(x=data[,targetcolumn], f=data$dose_fac, a=(alpha*2))
    levels <- levels(data$dose_fac)
    redlevels <- levels[levels != "0"]
    fullresultmat <- dunnett_t3[[2]]
    redresultmat <- fullresultmat[1:length(redlevels), 1:3] 
    
    outputmatrix <- matrix(nrow=(length(redlevels)+3), ncol=4)
    outputmatrix[1,1] <- "Dunnett's T3 Test"
    outputmatrix[3,1] <- "Dose Levels"
    outputmatrix[3,2] <- "Difference"
    outputmatrix[3,3] <- "Lower CI"
    outputmatrix[3,4] <- "Upper CI"
    
    for (i in 1:length(redlevels)) {
        
        outputmatrix[i+3,1] <- strtrim(levels[[i+1]], 6)
        outputmatrix[i+3,2] <- round(redresultmat[i,1], digits = 4)
        outputmatrix[i+3,3] <- round(redresultmat[i,2], digits = 4)
        outputmatrix[i+3,4] <- round(redresultmat[i,3], digits = 4)
    }
    
    outputlist <- list()
    outputlist[[1]] <- outputmatrix
	return(outputlist)
}
