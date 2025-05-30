% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fcst_utils.R
\name{ptd_gr}
\alias{ptd_gr}
\title{Period to date growth rate}
\usage{
ptd_gr(x, per = "year", lag_length = "1 year")
}
\arguments{
\item{x}{a ts-boxable object}

\item{per}{period over which the sum or average of a higher frequency series in x is calculated
(this is the unit of time at which the aggregation is performed:
for ytd per = "year" (default), for mtd per = "month")}

\item{lag_length}{lag over which growth is calculated or time difference relative to base period
(e.g. "1 year" (default), "3 years", etc. See ?ts_lag() for options)}
}
\value{
object of the same type as the input containing period to date growth rate
}
\description{
Period to date growth rate
}
\examples{
monthly_data_example |>
  ptd_gr() |>
  tail()
monthly_data_example |>
  dplyr::select(time, "VAPNS_HI") |>
  ptd_gr(per = "month", lag_length = "3 years") |>
  tail()
# don't use lag_length = "1 year" with weekly data
daily_data_example |>
  ptd_gr("week")
# lag_length = "52 weeks" instead
daily_data_example |>
  ptd_gr("week", "52 weeks")
# and use lag_length = "364 days" with daily data
daily_data_example |>
  ptd_gr("day", "364 days")
daily_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_pick("VAPNS_HI") |>
  ptd_gr("week", "4 weeks") \%>\%
  tail()
}
