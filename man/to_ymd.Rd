% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fcst_utils.R
\name{to_ymd}
\alias{to_ymd}
\title{Parse strings into dates in yyyy-mm-dd format}
\usage{
to_ymd(x)
}
\arguments{
\item{x}{string (string: yyyymmdd, yyyyqq, yyyy.q, yyyy)}
}
\value{
formatted dates (yyyy-mm-dd)
}
\description{
Parse strings into dates in yyyy-mm-dd format
}
\examples{
to_ymd(c("2010.0211", 202002, 2020.2, "2020"))
}
