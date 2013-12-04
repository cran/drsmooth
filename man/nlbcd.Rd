\name{nlbcd}
\alias{nlbcd}
\title{Non-linearity Below Cut-off Dose}
\usage{
  nlbcd (dosecolumn = "", targetcolumn = "", cutoffdose =
    0, data = NA)
}
\arguments{
  \item{dosecolumn}{Name of dose column in input
  dataframe.}

  \item{targetcolumn}{Name of response column in input
  dataframe.}

  \item{cutoffdose}{Numeric tested cut-off dose.}

  \item{data}{Input dataframe.}
}
\value{
  The analysis of variance table comparing the non-linear
  spline model with the linear model to assess
  non-linearity across doses below the user-specified
  cutoff.
}
\description{
  This function tests non-linearity below a specified dose.
}
\details{
  The user may provide a limit below which the
  non-linearity of the dose-response relationship is
  tested.  A significant result indicates that the
  dose-response relationship exhibits non-linearity below
  the user-specified cutoff dose.  NOTE: The dose-response
  relationship estimated by this function is not necesarily
  the same as that estimated by the nlaad function, as the
  nlbcd only uses doses below the cutoff and nlaad uses all
  doses. The user should keep this in mind in interpreting
  the outputs of these functions.
}
\examples{
# Prints the F test of the difference between the spline model (output "Model 2")
# and the linear model (output "Model 1") as a test of nonlinearity
# for doses below 1.5 (i.e., all dose levels up to and including 1.49):
nlbcd("dose", "MF_Log", cutoffdose=1.5, data=DRdata)

# This produces an error, as no cutoffdose was specified:
\dontrun{
nlbcd("dose", "MF_Log", data=DRdata)
}
}

