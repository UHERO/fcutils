% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fcst_utils.R
\name{cagr}
\alias{cagr}
\title{Calculate compund annual growth}
\usage{
cagr(x)
}
\arguments{
\item{x}{ts-boxable object for which growth is calculated between first and last period}
}
\value{
a tibble with a single row containing the compound annual growth between
the first and last period of x (in percent)
}
\description{
Calculate compund annual growth
}
\examples{
quarterly_data_example |>
  cagr()
quarterly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_xts() |>
  cagr()
quarterly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_xts() |>
  tsbox::ts_span("2000-01-01", "2020-01-01") |>
  tsbox::ts_pick("E_NF_HI") |>
  cagr()
}
