#' @name spline.plot
#' @title Plot Spline
#' @usage
#' spline.plot(dosecolumn = "", targetcolumn = "", data = NA)
#' @description
#' This function generates a spline model with the input dose and target response
#' columns, and plots the spline-estimated dose-response function with its upper and lower
#' 95 percent confidence bounds in green and red respectively along with the actual data.
#' Note that the confidence bounds depicted on the plot are for the dose-response function
#' itself, and not for the raw data.
#' @param dosecolumn   Name of dose column.
#' @param targetcolumn   Name of response column.
#' @param data   Input dataframe.
#' @return A plot of the spline-estimated dose-response function along with the actual data.
#' @examples
#' # Produces and plots the spline model with confidence bounds.
#' #For the same plot with key metrics, see drsmooth().
#' spline.plot("dose", "MF_Log", data=DRdata) 
#' @export

spline.plot <- function (dosecolumn="", targetcolumn="", data=NA) {

    x <- data
	targetvariable <- x[,targetcolumn]
	dose <- x[,dosecolumn]
	spline <- gam(targetvariable~s(dose, k=4), data=x)
   
	min <- min(dose)
	max <- max(dose)
	step <- (max-min)/1000
    predictdata <- data.frame(cbind(dose = seq(min, max, by = step)))
    
    alpha <- .10
    CIfactor <- qt(c(alpha/2), df = spline$df.residual, lower.tail=FALSE)
	predictnew <- predict(spline, predictdata, type="response", se.fit=TRUE)
	predictdata$fit <- as.vector(predictnew$fit)
	predictdata$se <- as.vector(predictnew$se)
	predictdata$lcl95 <- predictdata$fit-(CIfactor*predictdata$se)
	predictdata$ucl95 <- predictdata$fit+(CIfactor*predictdata$se)
	
    matplot(predictdata$dose, predictdata[, c("fit","lcl95","ucl95")], type="l", pch=19, lty=1, ylim=c(min(data[,targetcolumn]),max(data[,targetcolumn])),
            xlab="Dose", ylab="Predictions, CIs, and Raw Data")
    matpoints(x[,dosecolumn], x[,targetcolumn], type="p", pch=19)
}