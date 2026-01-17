# fcutils

The goal of fcutils is to facilitate time series data wrangling. The
package contains utility functions for time series generation, frequency
conversion, growth rate calculation, plotting, and more.

## Installation

You can install the development version of fcutils from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("UHERO/fcutils")
```

## Example

Illustration of some package features.

``` r
# load the fcutils package
library(fcutils)

# get public data from UDAMAN
get_series(c("VADCNS@HI.Q", "EAFNS@HI.Q", "YLAF@HI.Q", "PRM@HI.Q"), raw  = TRUE, public = TRUE) |> 
  # calculate year-to-date growth rate
  ytd_gr() |>
  # filter for the period 2017Q1 to 2023Q4
  span(2017.1, 2023.4) |>
  # convert to monthly frequency
  disagg(conv_type = "mean", target_freq = "month") |>
  # plot the series, with the last one on the secondary axis
  plot_2ax()
```
