#' @name dunns2
#' @title Dunn's Two-tailed
#' @param targetcolumn  Character string, name of response column to be tested
#' @param alpha  Significance level (numeric) to be used
#' @param direction  Direction of the anticipated difference
#' @param data  Input dataframe.
#' @keywords internal
#' @export

dunns2 <- function (targetcolumn, alpha, direction, data) {
	kruskal2 <- kruskalmc(data[,targetcolumn], data$dose_fac, probs=alpha, "two-tailed")
    output <- dunns.format(kruskal2, "Dunn's Two-tailed Test")
	return(output)
}
