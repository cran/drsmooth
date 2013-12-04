#' @title Dose-response Modeling with Smoothing Splines
#' @usage
#' smooth(dosecolumn = "", targetcolumn = "", data = NA)
#' @aliases smooth
#' @description
#' Generates a spline model given dose and target response columns.
#' @details
#' This function generates a spline model with the input dose and target response
#' columns, plots the spline-estimated dose-response function with its upper and lower
#' 95 percent confidence bounds in green and red respectively along with the actual data, and returns
#' key metrics related to the dose-response function.  Note that the confidence bounds depicted on the
#' plot are for the dose-response function itself, and not for the raw data.
#' @param dosecolumn   Name of dose column of interest in dataframe.
#' @param targetcolumn  Name of response column of interest in dataframe.
#' @param data   Input dataframe.
#' @return A plot of the spline-estimated dose-response function along with the actual data.
#' Also, several key metrics are reported:
#'
#' STD (slope transition dose): The lowest dose at which the slope of the dose-response function
#' is significantly (90% two-sided) positive.
#'
#' STD_l and STD_u: The 90 percent lower and upper confidence bounds on the STD.
#'
#' STD_bias (experimental): An estimate of the bias associated with the STD.
#'
#' iLOGEL (experimental: interpolated lowest observed effect level) The lowest dose at which the predicted response
#' exceeds the 90 percent upper confidence bound of the response at zero dose.  This value
#' can be anywhere within the dose range -- hence "interpolated."
#'
#' iLOGEL_l and iLOGEL_u: The 90 percent lower and upper confidence bounds on the iLOGEL.
#' @examples
#' # Produces and plots spline model with confidence bounds, and prints key metrics.
#' # For the plot only, see spline.plot
#' # Due to run-time, this example is documented as "don't run."
#' \dontrun{
#' smooth("dose", "MF_Log", data=DRdata)
#' }
#' @export

smooth <- function (dosecolumn="", targetcolumn="", data=NA) {
    
    x <- data
	targetvariable <- x[,targetcolumn]
	dose <- x[,dosecolumn]
    alpha <- .10
    
	spline <- gam(targetvariable~s(dose, k=4), data=x)
    
	min <- min(dose)
	max <- max(dose)
	step <- (max-min)/1000
    
	predictdata <- data.frame(cbind(dose = seq(min, max, by = step)))
	CIfactor <- qt(c(alpha/2), df = spline$df.residual, lower.tail=FALSE)
	predictnew <- predict(spline, predictdata, type="response", se.fit=TRUE)
	predictdata$fit <- as.vector(predictnew$fit)
	predictdata$se <- as.vector(predictnew$se)
	predictdata$lcl90 <- predictdata$fit-(CIfactor*predictdata$se)
	predictdata$ucl90 <- predictdata$fit+(CIfactor*predictdata$se)
    
	d1_spline <- firstDeriv(spline, n = 1001)
    
	predictdata$d1 <- as.vector(d1_spline$dose$deriv)
	predictdata$d1se <- as.vector(d1_spline$dose$se.deriv)
	predictdata$d1_lcl90 <- as.vector(predictdata$d1-(CIfactor*predictdata$d1se))
	predictdata$d1_ucl90 <- as.vector(predictdata$d1+(CIfactor*predictdata$d1se))
    
	predict <- predict(spline, type="response", se.fit=TRUE)
	x$fit <- as.vector(predict$fit)
	x$se <- as.vector(predict$se)
	x$lcl90 <- x$fit-(CIfactor*x$se)
	x$ucl90 <- x$fit+(CIfactor*x$se)
    
	zerodose90ucl <- predictdata$ucl90[1]
	loaellogical <- which(predictdata$fit>zerodose90ucl)
	loael <- round(predictdata$dose[min(loaellogical)], digits = 4)
    
    if (is.na(loael)) {
        warning('No ST dose can be identified in these data.')
        matplot(predictdata[,dosecolumn], predictdata[, c("fit","lcl90","ucl90")], type="l", pch=19, lty=1, ylim=c(min(x[,targetcolumn]),max(x[,targetcolumn])),
        xlab="Dose", ylab="Predictions, CIs, and Raw Data")
        matpoints(x[,dosecolumn], x[,targetcolumn], type="p", pch=19)
    } else {
        
        lclloaellogical <- which(predictdata$ucl90>zerodose90ucl)
        lcl90loael <- round(predictdata$dose[min(lclloaellogical)], digits = 4)
        
        uclloaellogical <- which(predictdata$lcl90>zerodose90ucl)
        ucl90loael <- round(predictdata$dose[min(uclloaellogical)], digits = 4)
        
        tdlogical <- which(predictdata$d1_lcl90>0)
        td <- round(predictdata$dose[min(tdlogical)], digits = 4)
        
        n <- nrow(x)
        predictdata$se_of_d1se <- predictdata$d1se/(n^.5)
        predictdata$d1_lcl90_u <- predictdata$d1_lcl90+(1.644854*predictdata$se_of_d1se)
        predictdata$d1_lcl90_l <- predictdata$d1_lcl90-(1.644854*predictdata$se_of_d1se)
        
        lcl90tdlogical <- which(predictdata$d1_lcl90_u>0)
        lcl90td <- round(predictdata$dose[min(lcl90tdlogical)], digits = 4)
        
        ucl90tdlogical <- which(predictdata$d1_lcl90_l>0)
        ucl90td <- round(predictdata$dose[min(ucl90tdlogical)], digits = 4)
        
        
        
        # The following line initializes a vector for storing the td values from the bootstrap process to follow.
        
        tdsampleresults <- as.vector(rep(NA, 1000))
        
        # And now the loop to perform the bootstrap.
        
        for (i in 1:1000) {
            
            sampledata <- x[sample(nrow(x),replace=TRUE),]
            sampletargetvariable <- sampledata[,targetcolumn]
            sampledosevariable <- sampledata[,dosecolumn]
            
            samplespline <- gam(sampletargetvariable~s(dose, k=4), data=sampledata)
            
            samplemin <- min(sampledata[,dosecolumn])
            samplemax <- max(sampledata[,dosecolumn])
            samplestep <- (samplemax-samplemin)/1000
            predictsampledata <- data.frame(cbind(dose = seq(samplemin, samplemax, by = samplestep)))
            CIfactor <- qt(c(alpha/2), df = samplespline$df.residual, lower.tail=FALSE)
            predictsamplenew <- predict(samplespline, predictsampledata, type="response", se.fit=TRUE)
            predictsampledata$fit <- as.vector(predictsamplenew$fit)
            predictsampledata$se <- as.vector(predictsamplenew$se)
            
            predictsampledata$lcl90 <- predictsampledata$fit-(CIfactor*predictsampledata$se)
            predictsampledata$ucl90 <- predictsampledata$fit+(CIfactor*predictsampledata$se)
            
            d1_samplespline <- firstDeriv(samplespline, n = 1001)
            
            predictsampledata$d1 <- as.vector(d1_samplespline$dose$deriv)
            predictsampledata$d1se <- as.vector(d1_samplespline$dose$se.deriv)
            predictsampledata$d1_lcl90 <- as.vector(predictsampledata$d1-(CIfactor*predictsampledata$d1se))
            predictsampledata$d1_ucl90 <- as.vector(predictsampledata$d1+(CIfactor*predictsampledata$d1se))
            
            tdsamplelogical <- which(predictsampledata$d1_lcl90>0)
            tdsample <- predictsampledata$dose[min(tdsamplelogical)]
            
            tdsampleresults[i] <- tdsample
            
		}
        
        tdsampleresults_s <- sort(tdsampleresults)
        
        tdbias <- round(td-mean(tdsampleresults_s), digits = 3)
        
        matplot(predictdata[,dosecolumn], predictdata[, c("fit","lcl90","ucl90")], type="l", pch=19, lty=1, ylim=c(min(x[,targetcolumn]),max(x[,targetcolumn])),
        xlab="Dose", ylab="Predictions, CIs, and Raw Data")
        matpoints(x[,dosecolumn], x[,targetcolumn], type="p", pch=19)
        
        output <- c("STD"=td, "STD_l"=lcl90td, "STD_u"=ucl90td, "STD_bias"=tdbias, "iLOGEL"=loael, "iLOGEL_l"=lcl90loael, "iLOGEL_u"=ucl90loael)
        
        print(output)
    }
    
}

