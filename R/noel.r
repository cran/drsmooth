#' @name noel
#' @aliases noel
#' @title No/Lowest Observed Effect Levels
#' @usage
#' noel(dosecolumn = "", 
#'       targetcolumn = "", 
#'       tests = c("dunnetts1", "dunnetts2", "dunns1", "dunns2", "dunnettst3"),
#'       direction = "greater",
#'       alpha = .05,
#'       data = NA)
#' @description
#' This function calculates and displays the results of the requested no/lowest observed effect level tests.
#' @details
#' Dunnett's one-tailed, Dunnett's two-tailed, Dunn's one-tailed, Dunn's two-tailed, and Dunnett's T3
#' tests are all executed, unless a subset of these tests is specified by replacing the default list
#' with a subset of the available tests.
#' Dosecolumn should be assigned the name of the dose column in the input dataframe, e.g. "dose".
#' Targetcolumn should be assigned the name of the response column in the input dataframe, e.g. "resp".
#' If the direction of the dose-response relation is expected to be negative, direction can be specified as "less".
#' The alpha level determining significance can be specified.
#' Data must be assigned the input dataframe.
#' @param dosecolumn  Character string, name of dose column to be tested
#' @param targetcolumn  Character string, name of response column to be tested
#' @param tests  List of tests to run.  Specify a subset by omitting any tests not desired:
#' c("dunnetts1", "dunnetts2", "dunns1", "dunns2", "dunnettst3")
#' @param direction  Direction of the anticipated difference
#' @param alpha  Significance level (numeric) to be used
#' @param data  Input dataframe.
#' @return
#' Shown are tables giving the comparisons of the active dose levels to the zero dose control
#' along with indications of significance specific to each type of test.
#' @examples
#' # Prints all available tests of no/lowest observed effect levels
#' # at default alpha=.05:
#' noel("dose", "MF_Log", data=DRdata) 
#'
#' # Prints only Dunnett's one-tailed and Dunn's 2-tailed tests at default alpha=.05:
#' noel("dose", "MF_Log", tests=c("dunnetts1", "dunns2"), data=DRdata) 
#'
#' # Shows Dunnett's T3 tests at user-specified alpha of .01
#' noel("dose", "MF_Log", tests=c("dunnettst3"), alpha=.01, data=DRdata) 
#' @export

noel <- function (dosecolumn = "", targetcolumn = "", tests = c("dunnetts1", "dunnetts2", "dunns1", "dunns2", "dunnettst3"), direction="greater", alpha=0.05, data=NA) {

	data <- dosefactor(dosecolumn, data)

	for (i in 1:length(tests)) {	
		test_name <- tests[i]
		f <- match.fun(test_name)
		result <- f(targetcolumn, alpha, direction, data)
        suppressWarnings(for (i in 1:length(result)) {
            if (class(result[[i]])=="matrix") {drsmooth.print(result[[i]])} else {print(result[[i]])}
        }
        )
    }
}
