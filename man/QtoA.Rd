% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fcst_utils.R
\name{QtoA}
\alias{QtoA}
\title{Aggregate from quarterly to annual frequency
(superseded by tsbox::ts_frequency())}
\usage{
QtoA(ser_in, aggr = "mean")
}
\arguments{
\item{ser_in}{the xts series to be converted (freq = q)}

\item{aggr}{aggregate via mean (default) or sum}
}
\value{
converted xts series (freq = a)
}
\description{
Aggregate from quarterly to annual frequency
(superseded by tsbox::ts_frequency())
}
\examples{
quarterly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_xts() |>
  tsbox::ts_pick("E_NF_HI") |>
  QtoA() |> # this matches with below
  AtoQ() |>
  QtoA() |> # this matches with above
  tsbox::ts_plot()
}
