\name{noel}
\alias{noel}
\title{No/Lowest Observed Effect Levels}
\usage{
  noel(dosecolumn = "", targetcolumn = "", tests =
    c("dunnetts1", "dunnetts2", "dunns1", "dunns2",
    "dunnettst3"), direction = "greater", alpha = .05, data
    = NA)
}
\arguments{
  \item{dosecolumn}{Character string, name of dose column
  to be tested}

  \item{targetcolumn}{Character string, name of response
  column to be tested}

  \item{tests}{List of tests to run.  Specify a subset by
  omitting any tests not desired: c("dunnetts1",
  "dunnetts2", "dunns1", "dunns2", "dunnettst3")}

  \item{direction}{Direction of the anticipated difference}

  \item{alpha}{Significance level (numeric) to be used}

  \item{data}{Input dataframe.}
}
\value{
  Shown are tables giving the comparisons of the active
  dose levels to the zero dose control along with
  indications of significance specific to each type of
  test.
}
\description{
  This function calculates and displays the results of the
  requested no/lowest observed effect level tests.
}
\details{
  Dunnett's one-tailed, Dunnett's two-tailed, Dunn's
  one-tailed, Dunn's two-tailed, and Dunnett's T3 tests are
  all executed, unless a subset of these tests is specified
  by replacing the default list with a subset of the
  available tests. Dosecolumn should be assigned the name
  of the dose column in the input dataframe, e.g. "dose".
  Targetcolumn should be assigned the name of the response
  column in the input dataframe, e.g. "resp". If the
  direction of the dose-response relation is expected to be
  negative, direction can be specified as "less". The alpha
  level determining significance can be specified. Data
  must be assigned the input dataframe.
}
\examples{
# Prints all available tests of no/lowest observed effect levels
# at default alpha=.05:
noel("dose", "MF_Log", data=DRdata)

# Prints only Dunnett's one-tailed and Dunn's 2-tailed tests at default alpha=.05:
noel("dose", "MF_Log", tests=c("dunnetts1", "dunns2"), data=DRdata)

# Shows Dunnett's T3 tests at user-specified alpha of .01
noel("dose", "MF_Log", tests=c("dunnettst3"), alpha=.01, data=DRdata)
}

