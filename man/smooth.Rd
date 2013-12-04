\name{smooth}
\alias{smooth}
\title{Dose-response Modeling with Smoothing Splines}
\usage{
  smooth(dosecolumn = "", targetcolumn = "", data = NA)
}
\arguments{
  \item{dosecolumn}{Name of dose column of interest in
  dataframe.}

  \item{targetcolumn}{Name of response column of interest
  in dataframe.}

  \item{data}{Input dataframe.}
}
\value{
  A plot of the spline-estimated dose-response function
  along with the actual data. Also, several key metrics are
  reported:

  STD (slope transition dose): The lowest dose at which the
  slope of the dose-response function is significantly (90%
  two-sided) positive.

  STD_l and STD_u: The 90 percent lower and upper
  confidence bounds on the STD.

  STD_bias (experimental): An estimate of the bias
  associated with the STD.

  iLOGEL (experimental: interpolated lowest observed effect
  level) The lowest dose at which the predicted response
  exceeds the 90 percent upper confidence bound of the
  response at zero dose.  This value can be anywhere within
  the dose range -- hence "interpolated."

  iLOGEL_l and iLOGEL_u: The 90 percent lower and upper
  confidence bounds on the iLOGEL.
}
\description{
  Generates a spline model given dose and target response
  columns.
}
\details{
  This function generates a spline model with the input
  dose and target response columns, plots the
  spline-estimated dose-response function with its upper
  and lower 95 percent confidence bounds in green and red
  respectively along with the actual data, and returns key
  metrics related to the dose-response function.  Note that
  the confidence bounds depicted on the plot are for the
  dose-response function itself, and not for the raw data.
}
\examples{
# Produces and plots spline model with confidence bounds, and prints key metrics.
# For the plot only, see spline.plot
# Due to run-time, this example is documented as "don't run."
\dontrun{
smooth("dose", "MF_Log", data=DRdata)
}
}

