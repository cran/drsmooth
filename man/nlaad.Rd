\name{nlaad}
\alias{nlaad}
\title{Non-linearity Across All Doses}
\usage{
  nlaad(dosecolumn = "", targetcolumn = "", data = NA)
}
\arguments{
  \item{dosecolumn}{Name of dose column in dataframe.}

  \item{targetcolumn}{Name of response column in
  dataframe.}

  \item{data}{Input dataframe.}
}
\value{
  The analysis of variance table comparing the non-linear
  spline model with the linear model to assess
  non-linearity across all doses.
}
\description{
  This function determines whether a non-linear spline
  model of the dose-response relationship differs
  significantly from a linear model across all doses.
}
\details{
  The non-linear spline model (output "Model 2") is
  compared to the linear model (output "Model 1") using an
  anova F-test.  If the spline model fits the data
  signficantly better, the F will be large and the
  associated p value will be significant.
}
\examples{
# Prints the F test of the difference between the spline model (output "Model 2")
# and the linear model (output "Model 1") as a test of nonlinearity
# across all doses:
nlaad("dose", "MF_Log", data=DRdata)
}

