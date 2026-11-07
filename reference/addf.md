# Create xts addfactor

Create xts addfactor

## Usage

``` r
addf(
  start = bank_start(),
  end = bank_end(),
  from = 0,
  to = 0,
  ser_name = "value",
  per = "year",
  ...
)
```

## Arguments

- start:

  start date of linear interpolation (character: "yyyy-mm-dd", "yyyyqq",
  "yyyy"), or a data frame of ramps with columns start, end, from, to
  (one ramp per row; see details)

- end:

  end date of linear interpolation (character: "yyyy-mm-dd", "yyyyqq",
  "yyyy")

- from:

  first value for linear interpolation (numeric)

- to:

  last value for linear interpolation (numeric)

- ser_name:

  name of the xts series (string)

- per:

  periodicity of series (character: "year" - default) if start is
  written as a quarter or a month (see
  [`to_ymd()`](https://uhero.github.io/fcutils/reference/to_ymd.md)),
  per is set to "quarter" or "month"

- ...:

  with formulas: further points (see details)

## Value

a single xts series spanning the bank: from
`getOption("fcutils.bnk_start")` to `getOption("fcutils.bnk_end")` (see
[`make_xts()`](https://uhero.github.io/fcutils/reference/make_xts.md))

## Details

this is a wrapper around make_xts with some additional functionality.
the start and end dates specify the span of the non-zero add-factor
value. the remaining period between start and end is filled with zeros.

Given a data frame of ramps, addf() builds one add-factor per row and
returns their sum, so overlapping ramps add up. A data frame with no
rows gives zeros over the bank. The dates take any form
[`to_ymd()`](https://uhero.github.io/fcutils/reference/to_ymd.md) reads,
decimal years included, but a column must be all numbers or all strings.

Given formulas, `addf(2026.3 ~ 0, 2027.4 ~ -0.3, 2030.4 ~ 0)`, the
add-factor runs through the points in straight lines, one equal step per
period, and is zero before the first point and after the last. Each
formula has a date on the left and a value on the right. The frequency
follows the first date (2026.3 is a quarter), unless `per` is given by
name; `ser_name` and `per` must be named. Decimal dates are quarters, so
monthly points need strings (`"2026M10" ~ 1`).

## Examples

``` r
options(fcutils.bnk_start = as.Date("1970-01-01"), fcutils.bnk_end = as.Date("2070-12-31"))
addf()
#>            value
#> 1970-01-01     0
#> 1971-01-01     0
#> 1972-01-01     0
#> 1973-01-01     0
#> 1974-01-01     0
#> 1975-01-01     0
#> 1976-01-01     0
#> 1977-01-01     0
#> 1978-01-01     0
#> 1979-01-01     0
#>        ...      
#> 2061-01-01     0
#> 2062-01-01     0
#> 2063-01-01     0
#> 2064-01-01     0
#> 2065-01-01     0
#> 2066-01-01     0
#> 2067-01-01     0
#> 2068-01-01     0
#> 2069-01-01     0
#> 2070-01-01     0
addf(201002, 201504, 1, 2)
#>            value
#> 1970-01-01     0
#> 1970-04-01     0
#> 1970-07-01     0
#> 1970-10-01     0
#> 1971-01-01     0
#> 1971-04-01     0
#> 1971-07-01     0
#> 1971-10-01     0
#> 1972-01-01     0
#> 1972-04-01     0
#>        ...      
#> 2068-07-01     0
#> 2068-10-01     0
#> 2069-01-01     0
#> 2069-04-01     0
#> 2069-07-01     0
#> 2069-10-01     0
#> 2070-01-01     0
#> 2070-04-01     0
#> 2070-07-01     0
#> 2070-10-01     0
addf(20100101, 20601201, 1, 2, per = "month")
#>            value
#> 1970-01-01     0
#> 1970-02-01     0
#> 1970-03-01     0
#> 1970-04-01     0
#> 1970-05-01     0
#> 1970-06-01     0
#> 1970-07-01     0
#> 1970-08-01     0
#> 1970-09-01     0
#> 1970-10-01     0
#>        ...      
#> 2070-03-01     0
#> 2070-04-01     0
#> 2070-05-01     0
#> 2070-06-01     0
#> 2070-07-01     0
#> 2070-08-01     0
#> 2070-09-01     0
#> 2070-10-01     0
#> 2070-11-01     0
#> 2070-12-01     0
addf(20100101, from = 1, to = 2, per = "quarter")
#>               value
#> 1970-01-01 0.000000
#> 1970-04-01 0.000000
#> 1970-07-01 0.000000
#> 1970-10-01 0.000000
#> 1971-01-01 0.000000
#> 1971-04-01 0.000000
#> 1971-07-01 0.000000
#> 1971-10-01 0.000000
#> 1972-01-01 0.000000
#> 1972-04-01 0.000000
#>        ...         
#> 2068-07-01 1.962963
#> 2068-10-01 1.967078
#> 2069-01-01 1.971193
#> 2069-04-01 1.975309
#> 2069-07-01 1.979424
#> 2069-10-01 1.983539
#> 2070-01-01 1.987654
#> 2070-04-01 1.991770
#> 2070-07-01 1.995885
#> 2070-10-01 2.000000
addf(2010.2, 2015.4, 1, 2, "ECT_HI")
#>            ECT_HI
#> 1970-01-01      0
#> 1970-04-01      0
#> 1970-07-01      0
#> 1970-10-01      0
#> 1971-01-01      0
#> 1971-04-01      0
#> 1971-07-01      0
#> 1971-10-01      0
#> 1972-01-01      0
#> 1972-04-01      0
#>        ...       
#> 2068-07-01      0
#> 2068-10-01      0
#> 2069-01-01      0
#> 2069-04-01      0
#> 2069-07-01      0
#> 2069-10-01      0
#> 2070-01-01      0
#> 2070-04-01      0
#> 2070-07-01      0
#> 2070-10-01      0
# several ramps at once
ramps <- tibble::tribble(
  ~start,   ~end,     ~from, ~to,
  "2026Q3", "2027Q4", 0,     -0.3,
  "2028Q1", "2030Q4", -0.3,  0
)
addf(ramps)
#>            value
#> 1970-01-01     0
#> 1970-04-01     0
#> 1970-07-01     0
#> 1970-10-01     0
#> 1971-01-01     0
#> 1971-04-01     0
#> 1971-07-01     0
#> 1971-10-01     0
#> 1972-01-01     0
#> 1972-04-01     0
#>        ...      
#> 2068-07-01     0
#> 2068-10-01     0
#> 2069-01-01     0
#> 2069-04-01     0
#> 2069-07-01     0
#> 2069-10-01     0
#> 2070-01-01     0
#> 2070-04-01     0
#> 2070-07-01     0
#> 2070-10-01     0
# the same with decimal dates
ramps <- tibble::tribble(
  ~start, ~end,   ~from, ~to,
  2026.3, 2027.4, 0,     -0.3,
  2028.1, 2030.4, -0.3,  0
)
addf(ramps)
#>            value
#> 1970-01-01     0
#> 1970-04-01     0
#> 1970-07-01     0
#> 1970-10-01     0
#> 1971-01-01     0
#> 1971-04-01     0
#> 1971-07-01     0
#> 1971-10-01     0
#> 1972-01-01     0
#> 1972-04-01     0
#>        ...      
#> 2068-07-01     0
#> 2068-10-01     0
#> 2069-01-01     0
#> 2069-04-01     0
#> 2069-07-01     0
#> 2069-10-01     0
#> 2070-01-01     0
#> 2070-04-01     0
#> 2070-07-01     0
#> 2070-10-01     0
# the same as points, with decimal or quoted dates
addf(2026.3 ~ 0, 2027.4 ~ -0.3, 2028.1 ~ -0.3, 2030.4 ~ 0)
#>            value
#> 1970-01-01     0
#> 1970-04-01     0
#> 1970-07-01     0
#> 1970-10-01     0
#> 1971-01-01     0
#> 1971-04-01     0
#> 1971-07-01     0
#> 1971-10-01     0
#> 1972-01-01     0
#> 1972-04-01     0
#>        ...      
#> 2068-07-01     0
#> 2068-10-01     0
#> 2069-01-01     0
#> 2069-04-01     0
#> 2069-07-01     0
#> 2069-10-01     0
#> 2070-01-01     0
#> 2070-04-01     0
#> 2070-07-01     0
#> 2070-10-01     0
addf("2026Q3" ~ 0, "2027Q4" ~ -0.3, "2028Q1" ~ -0.3, "2030Q4" ~ 0)
#>            value
#> 1970-01-01     0
#> 1970-04-01     0
#> 1970-07-01     0
#> 1970-10-01     0
#> 1971-01-01     0
#> 1971-04-01     0
#> 1971-07-01     0
#> 1971-10-01     0
#> 1972-01-01     0
#> 1972-04-01     0
#>        ...      
#> 2068-07-01     0
#> 2068-10-01     0
#> 2069-01-01     0
#> 2069-04-01     0
#> 2069-07-01     0
#> 2069-10-01     0
#> 2070-01-01     0
#> 2070-04-01     0
#> 2070-07-01     0
#> 2070-10-01     0
```
