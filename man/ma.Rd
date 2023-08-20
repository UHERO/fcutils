% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fcst_utils.R
\name{ma}
\alias{ma}
\title{Backward looking moving average}
\usage{
ma(ser, ord)
}
\arguments{
\item{ser}{series (xts, long or wide tbl)}

\item{ord}{numeric order (window length) of moving average}
}
\value{
object with same class as input containing moving average
}
\description{
Backward looking moving average
}
\examples{
test <- make_xts(
  start = lubridate::ymd("2010-01-01"), end = lubridate::ymd("2015-01-01"),
  per = "quarter", val = 0:20
) |>
  magrittr::set_names(c("test"))
test <- test |>
  cbind(test * 2) |>
  magrittr::set_names(c("test1", "test2"))
test |> ma(3)
test |>
  tsbox::ts_tbl() |>
  ma(3)
test |>
  tsbox::ts_tbl() |>
  tsbox::ts_wide() |>
  ma(3)
}