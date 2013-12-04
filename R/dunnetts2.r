#' @name dunnetts2
#' @title Dunnett's Two-tailed
#' @param targetcolumn  Character string, name of response column to be tested
#' @param alpha  Significance level (numeric) to be used
#' @param direction  Direction of the anticipated difference
#' @param data  Input dataframe.
#' @keywords internal
#' @export

dunnetts2 <- function (targetcolumn, alpha, direction, data) {

	dose_fac <- data$dose_fac
    
	data.anova <- aov(data[,targetcolumn] ~ dose_fac, data=data)
	twotaildunnetts.glht <- glht(data.anova, linfct=mcp(dose_fac = "Dunnett"))
	dunnetts2summary <- summary(twotaildunnetts.glht)
    levels <- levels(data$dose_fac)
    output <- dunnetts.format(dunnetts2summary, "Dunnett's Two-tailed Multiple Comparisons Test", levels)
        
	return(output)
}
