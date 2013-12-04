#' @rdname dunns1
#' @title Dunn's One-tailed
#' @param targetcolumn  Character string, name of response column to be tested
#' @param alpha  Significance level (numeric) to be used
#' @param direction  Direction of the anticipated difference
#' @param data  Input dataframe.
#' @keywords internal
#' @export

dunns1 <- function (targetcolumn, alpha, direction, data) {
	kruskal1 <- kruskalmc(data[,targetcolumn], data$dose_fac, probs=alpha, "one-tailed")
    output <- dunns.format(kruskal1, "Dunn's One-tailed Test")
	return(output)
}