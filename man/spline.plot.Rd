\name{spline.plot}
\alias{spline.plot}
\title{Plot Spline}
\usage{
  spline.plot(dosecolumn = "", targetcolumn = "", data =
    NA)
}
\arguments{
  \item{dosecolumn}{Name of dose column.}

  \item{targetcolumn}{Name of response column.}

  \item{data}{Input dataframe.}
}
\value{
  A plot of the spline-estimated dose-response function
  along with the actual data.
}
\description{
  This function generates a spline model with the input
  dose and target response columns, and plots the
  spline-estimated dose-response function with its upper
  and lower 95 percent confidence bounds in green and red
  respectively along with the actual data. Note that the
  confidence bounds depicted on the plot are for the
  dose-response function itself, and not for the raw data.
}
\examples{
# Produces and plots the spline model with confidence bounds.
#For the same plot with key metrics, see drsmooth().
spline.plot("dose", "MF_Log", data=DRdata)
}

