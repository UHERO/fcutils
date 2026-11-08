# Building a forecast

A series’ *history* ends at its last observation. Forecasting it means
filling in the values from there to the end of the bank, from
assumptions about how it will grow or where it will be at certain dates.
fcutils calls this *extending* the series.

This article takes the Hawaii payroll jobs in `quarterly_data_example`,
whose history ends in 2025Q1, and extends them to 2035 with
[`extend()`](https://uhero.github.io/fcutils/reference/extend.md),
[`addf()`](https://uhero.github.io/fcutils/reference/addf.md),
[`disagg()`](https://uhero.github.io/fcutils/reference/disagg.md) and
[`multi_chain()`](https://uhero.github.io/fcutils/reference/multi_chain.md),
then averages the result to years with
[`aggr()`](https://uhero.github.io/fcutils/reference/aggr.md). The
assumptions are made up for the example. If the bank or the series names
are new to you, read [Getting
started](https://uhero.github.io/fcutils/articles/getting-started.md)
first.

``` r

library(fcutils)
options(
  fcutils.bnk_start = as.Date("1998-01-01"),
  fcutils.bnk_end = as.Date("2035-12-31")
)
q <- conv_xts(quarterly_data_example)
jobs <- q$E_NF_HI
```

Every series that
[`extend()`](https://uhero.github.io/fcutils/reference/extend.md) and
[`addf()`](https://uhero.github.io/fcutils/reference/addf.md) return
runs from the bank start to the bank end, so results line up with each
other and with the bank. Before the last observation they hold the
history; after it, the forecast.

## Growth through anchors

An *anchor* is a target value at a date. With `type = "growth"`, the
anchors are growth rates, and they are *annualized*: the growth over one
quarter, compounded over four quarters, so that 1.2 means a pace of 1.2%
a year.
[`extend()`](https://uhero.github.io/fcutils/reference/extend.md) draws
a straight line between the last observed growth rate and the first
anchor, and between each anchor and the next. It holds the last anchor
to the end of the bank. The level then grows from the last observation
at those rates.

Total jobs grew at an annualized 2.9% in 2025Q1. Here growth slows to
1.2% by 2025Q4 and to 0.8% by 2027Q4:

``` r

jobs_fc <- extend(jobs, 2025.4 ~ 1.2, 2027.4 ~ 0.8, type = "growth")
round(pcmp(jobs_fc, lag = 1, comp_freq = 4)[p(2025.1, 2028.1)], 2)
#>            E_NF_HI
#> 2025-01-01    2.92
#> 2025-04-01    2.35
#> 2025-07-01    1.78
#> 2025-10-01    1.20
#> 2026-01-01    1.15
#> 2026-04-01    1.10
#> 2026-07-01    1.05
#> 2026-10-01    1.00
#> 2027-01-01    0.95
#> 2027-04-01    0.90
#> 2027-07-01    0.85
#> 2027-10-01    0.80
#> 2028-01-01    0.80
```

Each anchor is a formula with the date on the left and the value on the
right. A decimal date is a quarter, so `2025.4` is 2025Q4. The same
anchors can be written with quoted dates, as a named vector, or with
`at` and `val`; all four calls give the same series:

``` r

extend(jobs, "2025Q4" ~ 1.2, "2027Q4" ~ 0.8, type = "growth")
extend(jobs, val = c("2025Q4" = 1.2, "2027Q4" = 0.8), type = "growth")
extend(jobs, at = c(2025.4, 2027.4), val = c(1.2, 0.8), type = "growth")
```

A monthly series needs quoted months (`"2026M10" ~ 1.2`), because R
reads `2026.10` as `2026.1`. Anchors dated at or before the last
observation are dropped, with a message naming them: history takes
precedence.

## A level through anchors

With the default `type = "level"`, the series itself runs through the
anchors. Construction’s share of jobs is 6.08% in 2025Q1. Here it rises
to 6.4% by 2026Q4 and eases to 6.0% by 2029Q4. `interp = "stine"` (the
Stineman spline) turns smoothly through the anchors without overshooting
them; the default, `"linear"`, draws straight lines.

``` r

share <- 100 * q$ECT_HI / jobs
paths <- tsbox::ts_c(
  linear = extend(share, 2026.4 ~ 6.4, 2029.4 ~ 6.0),
  stine = extend(share, 2026.4 ~ 6.4, 2029.4 ~ 6.0, interp = "stine")
)
tsbox::ts_plot(paths[p(2022, 2031)], title = "Construction share of jobs, %")
```

![Construction share of jobs, 2022 to 2031: history, then the linear
path with corners at the anchors and the smooth Stineman spline through
the same anchors.](forecasting_files/figure-html/unnamed-chunk-5-1.png)

## Add-factors from ramps

An add-factor is a series added to a forecast to shift it for a while.
[`addf()`](https://uhero.github.io/fcutils/reference/addf.md) builds one
from a table of *ramps*: each row is a straight line from `from` at
`start` to `to` at `end`, and the add-factor is zero outside the ramps.
Overlapping ramps add up. Here a project raises the construction share
by up to 0.3 points over 2026–2029:

``` r

ramps <- tibble::tribble(
  ~start, ~end,   ~from, ~to,
  2026.1, 2027.4, 0,     0.3,
  2028.1, 2029.4, 0.3,   0
)
adj <- addf(ramps, per = "quarter")
```

The dates can also be strings (`"2026Q1"`), and the same add-factor can
be written as points joined by straight lines:

``` r

ramps <- tibble::tribble(
  ~start,   ~end,     ~from, ~to,
  "2026Q1", "2027Q4", 0,     0.3,
  "2028Q1", "2029Q4", 0.3,   0
)
addf(2026.1 ~ 0, 2027.4 ~ 0.3, 2028.1 ~ 0.3, 2029.4 ~ 0)
```

Construction jobs follow from the adjusted share and the total. Because
all three series span the bank, ordinary arithmetic lines them up:

``` r

share_fc <- extend(share, 2026.4 ~ 6.4, 2029.4 ~ 6.0, interp = "stine") + adj
ct_fc <- share_fc / 100 * jobs_fc
round(ct_fc[p(2025.1, 2026.4)], 2)
#>            ECT_HI
#> 2025-01-01  39.40
#> 2025-04-01  39.75
#> 2025-07-01  40.23
#> 2025-10-01  40.75
#> 2026-01-01  41.30
#> 2026-04-01  42.10
#> 2026-07-01  42.79
#> 2026-10-01  43.32
```

## Following a projection

Sometimes the forecast for a series comes from elsewhere, such as a
projection by another agency. Its level may not match UHERO’s history,
for example because it uses a different definition. With
`type = "chain"`,
[`extend()`](https://uhero.github.io/fcutils/reference/extend.md) takes
only the growth of the projection, `path`, and applies it to the series
from its last observation. Suppose an annual projection of government
jobs for 2025–2035:

``` r

gv_a <- make_xts(2025, val = seq(128.5, by = 0.8, length.out = 11))
```

The history is quarterly, so the projection must be quarterly too.
[`disagg()`](https://uhero.github.io/fcutils/reference/disagg.md)
interpolates it to quarters whose averages equal the annual values.
Without anything to follow at the end of the series, the interpolation
bends toward zero growth over the last year;
`extend_with = "last_growth"` prevents that by continuing the series at
its last growth rate before interpolating.

``` r

gv_q <- disagg(gv_a, extend_with = "last_growth")
round(aggr(gv_q)[p(2025, 2027)], 3)
#>            value
#> 2025-01-01 128.5
#> 2026-01-01 129.3
#> 2027-01-01 130.1
```

[`extend()`](https://uhero.github.io/fcutils/reference/extend.md) then
continues the quarterly history with the growth of the interpolated
projection:

``` r

gv_fc <- extend(q$EGV_HI, path = gv_q, type = "chain")
gv <- tsbox::ts_c(history = q$EGV_HI, projection = gv_q, forecast = gv_fc)
round(gv[p(2024.4, 2025.4)], 2)
#>            history projection forecast
#> 2024-10-01  126.80         NA   126.80
#> 2025-01-01  128.17     128.34   128.17
#> 2025-04-01      NA     128.41   128.23
#> 2025-07-01      NA     128.53   128.35
#> 2025-10-01      NA     128.72   128.54
```

`join = "shift"` instead moves the projection by a constant to meet the
history and uses the growth of the shifted series.

## Updating a forecast bank

[`multi_chain()`](https://uhero.github.io/fcutils/reference/multi_chain.md)
does the same for many series at once. A common use is updating a
forecast when new data arrive. Suppose last time’s forecast was built on
history to 2023Q4, and the data now run to 2025Q1. Rather than set every
assumption again, keep the growth that the old forecast had for each
series, and apply it from the new last observation.

The old forecast here is made up: each series extended from 2023Q4 at 1%
annualized growth.

``` r

old_hist <- q[p("", 2023.4)]
old_fc <- do.call(cbind, lapply(names(old_hist), function(n) {
  extend(old_hist[, n], 2024.1 ~ 1, type = "growth")
}))
names(old_fc) <- names(old_hist)

new_fc <- multi_chain(q, old_fc, direction = "forward", verbose = FALSE)
round(new_fc[p(2024.4, 2025.3), 1:3], 2)
#>            E_NF_HI ECT_HI EMN_HI
#> 2024-10-01  643.26  39.11  12.98
#> 2025-01-01  647.90  39.40  13.16
#> 2025-04-01  649.52  39.50  13.20
#> 2025-07-01  651.13  39.59  13.23
```

All 30 series keep their new history to 2025Q1 and then grow at 1%, from
the new levels. Each series in the history is paired with the series of
the same name in the old forecast. If the names differ in their endings,
such as `UR_AT_US_A` (annual) and `UR_AT_US_SOLQ` (quarterly model
output), `match = "stem"` pairs them on the part before the ending. For
rates such as the unemployment rate, `method = "diff"` continues with
the change in points instead of the growth rate.
`direction = "backward"` extends a short history back in time with the
growth of a longer series.

## Annual results

Forecasts are usually reported as annual figures.
[`aggr()`](https://uhero.github.io/fcutils/reference/aggr.md) averages
the quarters of each complete year. Only complete years count, so a year
with three quarters of data has no value. Jobs are a count at a point in
time, so the average is right. For flows, such as visitor arrivals, the
annual figure is the total, so use `conv_type = "sum"`:

``` r

fc <- tsbox::ts_c(E_NF_HI = jobs_fc, ECT_HI = ct_fc, EGV_HI = gv_fc)
round(aggr(fc)[p(2024, 2028)], 1)
#>            E_NF_HI ECT_HI EGV_HI
#> 2024-01-01   639.1   39.2  125.7
#> 2025-01-01   652.7   40.0  128.3
#> 2026-01-01   661.0   42.4  129.1
#> 2027-01-01   667.3   44.0  129.9
#> 2028-01-01   672.7   43.1  130.7
```

When an annual history differs from the average of its quarters (because
it averages unadjusted months, say),
`multi_chain(annual_history, aggr(fc), direction = "forward")` keeps the
annual history and continues it with the growth of the annual averages
of the forecast.

[Growth rates, tables and
plots](https://uhero.github.io/fcutils/articles/growth-and-reporting.md)
shows how to look at the result.
