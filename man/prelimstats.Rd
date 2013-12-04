\name{prelimstats}
\alias{prelimstats}
\title{Preliminary Statistics}
\usage{
  prelimstats(dosecolumn="", tests=c("outlier", "bartlett",
    "shapiro", "chisquare", "jonckheere"), data=NA)
}
\arguments{
  \item{dosecolumn}{Name of column containing dose in input
  data frame, e.g. "dose"}

  \item{tests}{List of tests to run.  May specify a subset
  by omitting any of the default tests = c("outlier",
  "bartlett", "shapiro", "chisquare", "jonckheere").}

  \item{data}{Input dataframe.}
}
\value{
  Shown are p values for the homogeneity, normality, and
  trend tests, and the Bonferroni-adjusted p value for the
  most outlierly case.
}
\description{
  This function calculates and displays the p values for
  the requested distribution tests.
}
\details{
  Outlier (Bonferroni Outlier Test), homogeneity
  (Bartlett's), normality (Shapiro-Wilk), composite
  homogeneity/normality (Fisher chi-square combining
  Bartlett's and Shapiro-Wilk), and Jonckeere's (monotone
  trend) tests are available.  All tests are executed
  unless a smaller set is specified using the 'tests'
  parameter.

  Outlier test.  Calls outlierTest -- there is at least one
  Bonferroni-adjusted outlier if the p value is less than
  the targeted alpha level.

  Bartletts.  Variances are non-homogeneous if the p value
  is less than the targeted alpha level.

  Shapiro-Wilk.  The variable is non-normally distributed
  if the p-value is less than the targeted alpha level.

  Chisquare.  Fisher's combined p value for Bartlett's and
  Shapiro-Wilk tests.  This indexes the conformance of the
  outcome and its transformations to both normality and
  variance homogeneity.  Generally, the response
  transformation associated with the least-significant
  (highest p-value) is the most desirable transformation.

  Jonckheere.  There is evidence of a monotonic trend if
  the p-value is lower than the targeted alpha.
}
\examples{
# Prints all available preliminary tests:
prelimstats("dose", data=DRdata)

# Prints only the outlier test:
prelimstats("dose", tests="outlier", data=DRdata)

# Prints only the homogeneity and normality tests:
prelimstats("dose", tests=c("bartlett", "shapiro"), data=DRdata)
}

