#' @name test_nlaad
#' @title Test NLAAD Statistics

test_that("NLAAD Correct", {
    
    # test scripts do not seem to recognize loaded libraries:
    library(car)
    library(clinfun)
    library(multcomp)
    library(pgirmess)
    library(DTK)
    library(mgcv)
    library(segmented)

    targetcolumn <- "MF_Log"
    dosecolumn <- "dose"
    
    splinemod <- gam(DRdata[,targetcolumn]~s(DRdata[,dosecolumn], k=4), data=DRdata)
    linearmodel <- lm(DRdata[,targetcolumn]~DRdata[,dosecolumn], data=DRdata)
    output <- anova(linearmodel,splinemod,test="F")
    
    expect_that (round(output$F[2], digits = 5), equals(36.11283))

    }
)