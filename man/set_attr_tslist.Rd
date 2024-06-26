% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fcst_utils.R
\name{set_attr_tslist}
\alias{set_attr_tslist}
\title{Set class attribute to tslist}
\usage{
set_attr_tslist(x)
}
\arguments{
\item{x}{list, typically a result of purrr::map() applied to a tslist}
}
\value{
list with class attributes set to list and tslist
}
\description{
Set class attribute to tslist
}
\details{
A purrr::map() function applied to a tslist (obtained by tsbox::ts_tslist())
drops the tslist class attribute. This function resets that attribute.
}
\examples{
monthly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_tslist() |>
  purrr::map(~ .x / 1000) |>
  set_attr_tslist() |>
  tsbox::ts_tbl() |>
  tsbox::ts_wide()
}
