#' @name dunnetts1
#' @title Dunnett's One-tailed
#' @param targetcolumn  Character string, name of response column to be tested
#' @param alpha  Significance level (numeric) to be used
#' @param direction  Direction of the anticipated difference
#' @param data  Input dataframe.
#' @keywords internal
#' @export

dunnetts1 <- function (targetcolumn, alpha, direction, data) {

	dose_fac <- data$dose_fac
    
	data.anova <- aov(data[,targetcolumn] ~ dose_fac, data=data)
	onetaildunnetts.glht <- glht(data.anova, linfct=mcp(dose_fac = "Dunnett"), alternative=direction)
    
    # Retrieve simplified output
	dunnetts1summary <- summary(onetaildunnetts.glht)
    levels <- levels(data$dose_fac)
    output <- dunnetts.format(dunnetts1summary, "Dunnett's One-tailed Multiple Comparisons Test", levels)
    return(output)
}