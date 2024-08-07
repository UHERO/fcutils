% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fcst_utils.R
\name{set_tsrange}
\alias{set_tsrange}
\title{Set tsrange for behavioral equations to available data range}
\usage{
set_tsrange(model_w_dat, max_lag = 4, eqns = NULL)
}
\arguments{
\item{model_w_dat}{bimets model (with data) to be estimated}

\item{max_lag}{the largest lag (default = 4) in the model (to offset starting point for estimation)}

\item{eqns}{names of behavioral equations to set tsrange for (default = NULL: all equations)}
}
\value{
bimets model with tsrange set for estimation
}
\description{
Set tsrange for behavioral equations to available data range
}
\details{
Find periods where all variables in the equation are available.
Shift beginning of the sample by max_lag periods.
Set the tsrange for each equation (used in estimation).
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
set_tsrange(scen_model_dat, 4)
\dontshow{\}) # examplesIf}
}
