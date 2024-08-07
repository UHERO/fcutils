% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fcst_utils.R
\name{update_eqs}
\alias{update_eqs}
\title{Update a bimets model with new/modified equations}
\usage{
update_eqs(model_1, model_2, eqList)
}
\arguments{
\item{model_1}{original estimated bimets model}

\item{model_2}{estimated bimets model containing updates (only updated
equations need to be estimated)}

\item{eqList}{names of updated behavioral equations (vector of strings),
others taken from model_1 (equations missing from model_2 are removed)}
}
\value{
estimated bimets model containing updates
}
\description{
Update a bimets model with new/modified equations
}
\details{
Start by making a copy of the original model's equations (txt file).
Re-specify some equations, add new equations, and remove not needed equations.
Load the new model as model_2 and estimate the modified/new equations (ok to estimate all).
Replace the equations in model_2 that should remain the same as in model_1
by the estimated equations from model_1. Equations that are to remain unchanged
have to be present in both model_1 and model_2, and not present in eqList.
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
update_eqs(scen_model_1_est, scen_model_2_est, c("E_NF_AT_HI_Q", "Y_R_AT_HI_Q"))
\dontshow{\}) # examplesIf}
}
