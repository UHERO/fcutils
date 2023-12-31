% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fcst_utils.R
\name{get_var}
\alias{get_var}
\title{Construct a series name from variable components and retrieve the series}
\usage{
get_var(ser_in, env = parent.frame())
}
\arguments{
\item{ser_in}{a variable name (character string with substituted expressions)}

\item{env}{environment where the expression should be evaluated}
}
\value{
variable
}
\description{
Construct a series name from variable components and retrieve the series
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
ser_i <- "_NF"
cnty_i <- "HI"
quarterly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_xts() \%$\% get_var("E{ser_i}_{cnty_i}")
\dontshow{\}) # examplesIf}
}
