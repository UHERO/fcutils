# Create an add-factor

An add-factor is a series added to a forecast to shift it for a while.
`addf()` returns one that is zero over the bank except where you set it:
along a straight-line ramp, along several ramps that add up, or through
points given as formulas.

## Usage

``` r
addf(
  start = bank_start(),
  end = bank_end(),
  from = 0,
  to = 0,
  ser_name = "value",
  per = "year",
  ...,
  interp = c("linear", "stine")
)
```

## Arguments

- start:

  date the ramp starts, in any form
  [`to_ymd()`](https://uhero.github.io/fcutils/reference/to_ymd.md)
  reads ("2026Q3", 2026.3, "2026M07", 2026), or a data frame of ramps
  with columns start, end, from and to, one ramp per row. A day
  ("2026-07-01") must be the first day of a period at frequency `per`.

- end:

  date the ramp ends

- from:

  value at `start`

- to:

  value at `end`

- ser_name:

  name of the series

- per:

  frequency: "year" (default), "quarter" or "month". A start written as
  a quarter ("2026Q3", 2026.3) or a month ("2026M07") sets it.

- ...:

  with formulas: further points (see details)

- interp:

  with formulas: how to join the points, "linear" (default) or "stine",
  the Stineman spline, which eases in and out of each point without
  overshooting. Must be named.

## Value

one xts series over the bank, from `getOption("fcutils.bnk_start")` to
`getOption("fcutils.bnk_end")` (see
[`make_xts()`](https://uhero.github.io/fcutils/reference/make_xts.md)).
A ramp that starts before the bank or ends after it extends the series
past the bank.

## Details

A ramp runs in equal steps from `from` at `start` to `to` at `end`.
Before `start` and after `end` the add-factor is zero, so a ramp that
ends away from zero drops back to zero in the next period. With no
arguments, `addf()` gives zeros over the bank.

Given a data frame of ramps, `addf()` builds one add-factor per row and
returns their sum, so overlapping ramps add up. A data frame with no
rows gives zeros over the bank, at the frequency `per`. A date column
must be all numbers or all strings.

Given formulas, `addf(2026.3 ~ 0, 2027.4 ~ -0.3, 2030.4 ~ 0)`, the
add-factor runs through the points in straight lines, one equal step per
period, and is zero before the first point and after the last. Each
formula has a date on the left and a value on the right, and each date
must start a period within the bank. Give at least two points. The
frequency follows the date of the first formula (2026.3 is a quarter),
unless `per` is given by name; `ser_name` and `per` must be named.
Decimal dates are quarters, so monthly points need strings
(`"2026M10" ~ 1`).

With `interp = "stine"` the points are joined by a smooth curve instead.
The spline treats the zeros before the first point and after the last as
known, so an add-factor that starts and ends at zero leaves and returns
to zero gradually. A ramp joins only two points, so `interp` applies to
formulas, not to start, end, from and to or a table of ramps.

## Examples

``` r
options(
  fcutils.bnk_start = as.Date("2020-01-01"),
  fcutils.bnk_end = as.Date("2035-12-31")
)
# one ramp: from 0 in 2026Q3 to -0.3 in 2027Q4, zero outside
addf("2026Q3", "2027Q4", 0, -0.3)[p(2026.2, 2028.1)]
#>            value
#> 2026-04-01  0.00
#> 2026-07-01  0.00
#> 2026-10-01 -0.06
#> 2027-01-01 -0.12
#> 2027-04-01 -0.18
#> 2027-07-01 -0.24
#> 2027-10-01 -0.30
#> 2028-01-01  0.00
# a ramp that ends away from zero drops back to zero after its end
addf(2026.3, 2027.4, 1, 2, ser_name = "ECT_HI")[p(2027.3, 2028.2)]
#>            ECT_HI
#> 2027-07-01    1.8
#> 2027-10-01    2.0
#> 2028-01-01    0.0
#> 2028-04-01    0.0
# annual and monthly
addf(2026, 2028, 0, 1)[p(2025, 2029)]
#>            value
#> 2025-01-01   0.0
#> 2026-01-01   0.0
#> 2027-01-01   0.5
#> 2028-01-01   1.0
#> 2029-01-01   0.0
addf("2026M01", "2026M06", 0, 0.5)[p("2025M12", "2026M07")]
#>            value
#> 2025-12-01   0.0
#> 2026-01-01   0.0
#> 2026-02-01   0.1
#> 2026-03-01   0.2
#> 2026-04-01   0.3
#> 2026-05-01   0.4
#> 2026-06-01   0.5
#> 2026-07-01   0.0
# zeros over the bank
addf(per = "quarter")[p(2026.1, 2026.4)]
#>            value
#> 2026-01-01     0
#> 2026-04-01     0
#> 2026-07-01     0
#> 2026-10-01     0

# several ramps at once: down to -0.3 by 2027Q4, then back to 0 by 2030Q4
ramps <- tibble::tribble(
  ~start,   ~end,     ~from, ~to,
  "2026Q3", "2027Q4", 0,     -0.3,
  "2028Q1", "2030Q4", -0.3,  0
)
ramps_adj <- addf(ramps)
round(ramps_adj[p(2026.2, 2031.1)], 3)
#>             value
#> 2026-04-01  0.000
#> 2026-07-01  0.000
#> 2026-10-01 -0.060
#> 2027-01-01 -0.120
#> 2027-04-01 -0.180
#> 2027-07-01 -0.240
#> 2027-10-01 -0.300
#> 2028-01-01 -0.300
#> 2028-04-01 -0.273
#> 2028-07-01 -0.245
#> 2028-10-01 -0.218
#> 2029-01-01 -0.191
#> 2029-04-01 -0.164
#> 2029-07-01 -0.136
#> 2029-10-01 -0.109
#> 2030-01-01 -0.082
#> 2030-04-01 -0.055
#> 2030-07-01 -0.027
#> 2030-10-01  0.000
#> 2031-01-01  0.000
# the same as points, with decimal or quoted dates
pts_adj <- addf(2026.3 ~ 0, 2027.4 ~ -0.3, 2028.1 ~ -0.3, 2030.4 ~ 0)
all.equal(pts_adj, ramps_adj)
#> [1] TRUE
addf("2026Q3" ~ 0, "2027Q4" ~ -0.3, "2028Q1" ~ -0.3, "2030Q4" ~ 0)[p(2027.3, 2028.2)]
#>                 value
#> 2027-07-01 -0.2400000
#> 2027-10-01 -0.3000000
#> 2028-01-01 -0.3000000
#> 2028-04-01 -0.2727273
# a smooth curve through the same points
smooth_adj <- addf(2026.3 ~ 0, 2027.4 ~ -0.3, 2028.1 ~ -0.3, 2030.4 ~ 0,
                   interp = "stine")
round(tsbox::ts_c(linear = pts_adj, stine = smooth_adj)[p(2026.2, 2031.1)], 3)
#>            linear  stine
#> 2026-04-01  0.000  0.000
#> 2026-07-01  0.000  0.000
#> 2026-10-01 -0.060 -0.031
#> 2027-01-01 -0.120 -0.106
#> 2027-04-01 -0.180 -0.194
#> 2027-07-01 -0.240 -0.269
#> 2027-10-01 -0.300 -0.300
#> 2028-01-01 -0.300 -0.300
#> 2028-04-01 -0.273 -0.293
#> 2028-07-01 -0.245 -0.274
#> 2028-10-01 -0.218 -0.245
#> 2029-01-01 -0.191 -0.210
#> 2029-04-01 -0.164 -0.170
#> 2029-07-01 -0.136 -0.130
#> 2029-10-01 -0.109 -0.090
#> 2030-01-01 -0.082 -0.055
#> 2030-04-01 -0.055 -0.026
#> 2030-07-01 -0.027 -0.007
#> 2030-10-01  0.000  0.000
#> 2031-01-01  0.000  0.000
```
