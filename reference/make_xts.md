# Create xts and fill with values

Create xts and fill with values

## Usage

``` r
make_xts(start = bank_start(), end = NULL, per = "year", val = NA_real_)
```

## Arguments

- start:

  date of the first observation, in any form
  [`to_ymd()`](https://uhero.github.io/fcutils/reference/to_ymd.md)
  reads (default: the bank start)

- end:

  date of the last observation, in any form
  [`to_ymd()`](https://uhero.github.io/fcutils/reference/to_ymd.md)
  reads. Ignored when `val` is a tibble.

- per:

  periodicity of series (character: "year" - default) if start is
  written as a quarter or a month (see
  [`to_ymd()`](https://uhero.github.io/fcutils/reference/to_ymd.md)),
  per is set to "quarter" or "month"

- val:

  values to fill in (numeric scalar, vector, or tibble)

## Value

an xts object: one series, or one per column when `val` is a tibble

## Details

When end is missing and val is a vector of more than one element, the
length of val sets the end date. When end is missing and val is a
scalar, the series runs to the bank end, `getOption("fcutils.bnk_end")`.
When val is a tibble, its number of rows sets the end date. The default
start is the bank start, `getOption("fcutils.bnk_start")`. Set both once
per session, e.g. in setup.R:
`options(fcutils.bnk_start = as.Date("1970-01-01"), fcutils.bnk_end = as.Date("2070-12-31"))`.
Name the arguments after `start` unless you give `end`.

## Examples

``` r
options(fcutils.bnk_start = as.Date("1970-01-01"), fcutils.bnk_end = as.Date("2070-12-31"))
make_xts() |> head(3)
#>            value
#> 1970-01-01    NA
#> 1971-01-01    NA
#> 1972-01-01    NA
make_xts(val = 0, per = "m") |> head(3)
#>            value
#> 1970-01-01     0
#> 1970-02-01     0
#> 1970-03-01     0
make_xts(start = 20100101, per = "quarter", val = 0) |> head(3)
#>            value
#> 2010-01-01     0
#> 2010-04-01     0
#> 2010-07-01     0
make_xts(start = 2010.1, per = "q", val = 1:10) |> tail(3)
#>            value
#> 2011-10-01     8
#> 2012-01-01     9
#> 2012-04-01    10
make_xts(2010.1, val = 1:10) |> tail(3) # automatically set per = "quarter"
#>            value
#> 2011-10-01     8
#> 2012-01-01     9
#> 2012-04-01    10
make_xts(start = "2010-01-01", end = "2010-06-01", per = "m", val = 0)
#>            value
#> 2010-01-01     0
#> 2010-02-01     0
#> 2010-03-01     0
#> 2010-04-01     0
#> 2010-05-01     0
#> 2010-06-01     0
make_xts(start = 201001, per = "q",
         val = tibble::tibble(E_NF_HON = c(1:10), ECT_HI = c(11:20))) |> head(3)
#>            E_NF_HON ECT_HI
#> 2010-01-01        1     11
#> 2010-04-01        2     12
#> 2010-07-01        3     13
```
