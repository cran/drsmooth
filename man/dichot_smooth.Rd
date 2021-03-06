% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/dichot_smooth.R
\name{dichot_smooth}
\alias{dichot_smooth}
\title{Dose-response Modeling with Smoothing Splines}
\usage{
dichot_smooth(dosecolumn = "",
               targetcolumn = "",
               k = 4,
               return_predict = FALSE,
               write_predict = TRUE,
               STD_bias = TRUE,
               data = NA)
}
\arguments{
\item{dosecolumn}{Name of dose column of interest in dataframe.}

\item{targetcolumn}{Name of response column of interest in dataframe.}

\item{k}{Dimension of the basis used to represent the smooth term; see Details.}

\item{return_predict}{If TRUE (default FALSE), returns dataframe of predicted values.}

\item{write_predict}{If TRUE (the default), writes the dataframe of predicted values to a .csv file in the working directory.}

\item{STD_bias}{If TRUE (the default), calculates the slope transition dose, a bootstrapped and resource-intensive computation.}

\item{data}{Input dataframe.}
}
\value{
A plot of the spline-estimated dose-response function along with the actual data.
}
\description{
Generates a spline model given dose and target response columns.
}
\details{
This function generates a spline model with the input dose and target response
columns, plots the spline-estimated dose-response function with its upper and lower
95 percent confidence bounds in green and red respectively along with the actual data, and returns
key metrics related to the dose-response function.  Note that the confidence bounds depicted on the
plot are for the dose-response function itself, and not for the raw data.

The parameter 'k', defaulted to 4, defines the number of dimensions the spline function will use in
estimating the response relation.  With 2 reserved for each end of the smooth, the default
allows for 2 bends in the smooth.  In the case that this appears to overfit the data,
the user may choose to override the default to 3, which would allow only one bend.
}
\keyword{internal}

