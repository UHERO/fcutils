% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fcst_utils.R
\name{nmons}
\alias{nmons}
\title{Calculate number of months between two dates yyyyMm, yyyy.m or yyyy-mm-dd}
\usage{
nmons(dat1 = "", dat2 = "")
}
\arguments{
\item{dat1}{date of period start (string: yyyyMm, yyyy.m, or yyyy-mm-dd)}

\item{dat2}{date of period end (string: yyyyMm, yyyy.m, or yyyy-mm-dd)}
}
\value{
numeric length of date range in months
}
\description{
Calculate number of months between two dates yyyyMm, yyyy.m or yyyy-mm-dd
}
\details{
The endpoints are included in the result so subtract one for time difference.
Also, the result is rounded down so partial months are not counted. See examples.
}
\examples{
nmons("2010M1", "2010M2")
nmons(2010.1, 2010.4)
nmons("2010-01-15", "2010-04-15")
nmons("2010-01-15", "2010-04-18")
nmons("2010-01-15", "2010-04-12")
}
