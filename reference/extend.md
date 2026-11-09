# Extend a series past its last observation

Forecasts a single series: continues it from `last` to the end of the
bank, in one of three ways. The series runs through anchor values
(`type = "level"`); its growth rate runs through anchors, or follows a
growth path, and the level is compounded (`type = "growth"`); or it
continues with the growth or the changes of another series
(`type = "chain"`).

## Usage

``` r
extend(
  x,
  ...,
  at = NULL,
  val = NULL,
  path = NULL,
  type = c("level", "growth", "chain"),
  growth = c("pca", "pcy"),
  method = c("ratio", "diff"),
  interp = c("linear", "stine"),
  last = NULL,
  hold_last = TRUE,
  smooth = NULL
)
```

## Arguments

- x:

  a ts-boxable object holding one series (quarterly, monthly or annual)

- ...:

  anchors as formulas, date on the left and value on the right:
  `2026.4 ~ 4.4, 2028.4 ~ 4.0`. The right side is evaluated, so
  `2027.4 ~ target` works. An alternative to `at` and `val`.

- at:

  dates of the anchors, in any form
  [`to_ymd()`](https://uhero.github.io/fcutils/reference/to_ymd.md)
  reads (`c(2026.4, 2028.4)`, `c("2026Q4", "2028")`). Each must be the
  first day of a period of x within the bank, or extend() stops.

- val:

  anchor values, given by name: a numeric vector the length of `at`; a
  named numeric vector whose names are the dates (`c("2026Q4" = 4.3)`),
  with `at = NULL`; or a series (e.g. from
  [`make_xts()`](https://uhero.github.io/fcutils/reference/make_xts.md)),
  whose observations are the anchors (`at` is then ignored)

- path:

  instead of anchors, a series that sets the forecast: the level
  (`type = "level"`), the growth rate in percent (`type = "growth"`), or
  a level whose growth or changes continue x (`type = "chain"`). The
  forecast ends where the path ends.

- type:

  "level", "growth" or "chain" (see details)

- growth:

  for `type = "growth"`, how anchors and `path` state growth: "pca"
  (default), the period-on-period rate annualized (4.0 for a quarter
  that grew 1%); "pcy", the year-over-year rate

- method:

  for `type = "chain"`: "ratio" (default) continues x with the growth of
  `path`, for levels; "diff" with its changes, for rates (the same words
  as in
  [`chain()`](https://uhero.github.io/fcutils/reference/chain.md)).
  Ignored for other types.

- interp:

  interpolation between anchors: "linear" or "stine" (the Stineman
  spline: smooth turns without overshooting)

- last:

  last date that counts as history (default: the last observation of x).
  Values of x after `last` are replaced.

- hold_last:

  if TRUE (default), the last anchor is held to the end of the bank, so
  a spline flattens into it. If FALSE, the interpolation runs only to
  the last anchor and its value is then carried forward. With
  `interp = "linear"` both give the same result.

- smooth:

  NULL, or a list with `span` and optionally `from` and `to`: smooth the
  forecast with loess before compounding (the growth rate for "growth",
  year over year with `growth = "pcy"`; the period growth rate for
  "chain"; the level for "level"). `from` defaults to the first period
  after `last`, `to` to the end of the forecast.

## Value

an object of the same type as x, from the bank start to the bank end
(`getOption("fcutils.bnk_start")`, `getOption("fcutils.bnk_end")`),
empty before x's first observation. With a `path`, the forecast ends
where the path ends.

## Details

To join two sources of the same data, several series at once, or to
extend a series back in time, use
[`chain()`](https://uhero.github.io/fcutils/reference/chain.md) instead.
See "Which function" below.

- `type = "level"`: history up to `last`, then the anchors, with the
  gaps interpolated, including between the last history value and the
  first anchor. Anchors dated at or before `last` are dropped with a
  message, since history takes precedence. With a `path` instead, the
  forecast is the path's values after `last`, unadjusted, so it can jump
  at the seam; use `type = "chain"` to join it to history.

- `type = "growth"`: the same applied to the growth rate of x; the level
  is then compounded from history. With `growth = "pca"` the line starts
  from the annualized growth at `last` and each period grows from the
  one before (annualized at the series' frequency: 4 for quarters, 12
  for months). With `growth = "pcy"` it starts from the year-over-year
  growth at `last`, and each period grows from the same period a year
  earlier; this needs a full year of history and repeats last year's
  within-year pattern.

- `type = "chain"`: `path` after `last`, joined to x at `last`: scaled
  to meet it (`method = "ratio"`) or shifted by a constant
  (`method = "diff"`). The path needs a value at `last`. "ratio"
  compounds the path's growth, so the path must stay above zero. "diff"
  uses the shifted path as is, so it suits rates that reach zero or
  below; with `smooth`, it compounds the smoothed growth of the shifted
  path, which must then stay above zero.

Linear interpolation weighs the gaps by time, the spline by position.

**Which function.** All of these continue a series from data:

- levels at some dates: `extend(type = "level")`;

- growth rates at some dates, or a growth path, period-on-period or
  year-over-year: `extend(type = "growth", growth = "pca"/"pcy")`;

- another series' path (a projection, a model forecast), for one series:
  `extend(type = "chain", method = "ratio"/"diff")`;

- two sources of the same data to join, many series at once (each with
  its own method), or a series to extend back in time:
  [`chain()`](https://uhero.github.io/fcutils/reference/chain.md).

extend() returns the series on the bank; chain() keeps the span of its
inputs.

Anchors can be given in four ways: formulas (`2026.4 ~ 4.4`), `at` and
`val`, a named vector (`val = c("2026Q4" = 4.4)`), or a series. Decimal
dates such as `2026.4` are quarters, so a monthly series needs month
strings (`"2026M10" ~ 4.4`); extend() stops if a monthly series gets
decimal dates.

## Examples

``` r
options(fcutils.bnk_start = as.Date("2015-01-01"), fcutils.bnk_end = as.Date("2035-12-31"))
ur <- make_xts(2024.1, val = c(3.8, 4.0, 4.2, 4.1, 4.1, 4.2, 4.3))
# the unemployment rate through two anchors, then held
extend(ur, 2026.4 ~ 4.4, 2028.4 ~ 4.0, interp = "stine")[p(2025.2, 2029.1)]
#>               value
#> 2025-04-01 4.200000
#> 2025-07-01 4.300000
#> 2025-10-01 4.331334
#> 2026-01-01 4.354364
#> 2026-04-01 4.372436
#> 2026-07-01 4.387309
#> 2026-10-01 4.400000
#> 2027-01-01 4.389365
#> 2027-04-01 4.343685
#> 2027-07-01 4.276535
#> 2027-10-01 4.200000
#> 2028-01-01 4.124880
#> 2028-04-01 4.060852
#> 2028-07-01 4.016612
#> 2028-10-01 4.000000
#> 2029-01-01 4.000000
# the same with quoted dates, with at and val, or with a named vector
u1 <- extend(ur, "2026Q4" ~ 4.4, "2028Q4" ~ 4.0, interp = "stine")
u2 <- extend(ur, at = c(2026.4, 2028.4), val = c(4.4, 4.0), interp = "stine")
u3 <- extend(ur, val = c("2026Q4" = 4.4, "2028Q4" = 4.0), interp = "stine")
all.equal(u1, u2)
#> [1] TRUE
all.equal(u1, u3)
#> [1] TRUE
# a price level whose annualized inflation falls from 3% to 2%
cpi <- make_xts(2024.1, val = 100 * 1.0075^(0:6))
extend(cpi, at = c(2026.1, 2027.4), val = c(3, 2), type = "growth")[p(2025.2, 2028.4)]
#>               value
#> 2025-04-01 103.8067
#> 2025-07-01 104.5852
#> 2025-10-01 105.3653
#> 2026-01-01 106.1468
#> 2026-04-01 106.8974
#> 2026-07-01 107.6161
#> 2026-10-01 108.3015
#> 2027-01-01 108.9530
#> 2027-04-01 109.5706
#> 2027-07-01 110.1533
#> 2027-10-01 110.7000
#> 2028-01-01 111.2494
#> 2028-04-01 111.8015
#> 2028-07-01 112.3563
#> 2028-10-01 112.9140
# the same with year-over-year inflation
extend(cpi, at = c(2026.1, 2027.4), val = c(3, 2), type = "growth",
  growth = "pcy")[p(2025.2, 2028.4)]
#>               value
#> 2025-04-01 103.8067
#> 2025-07-01 104.5852
#> 2025-10-01 105.3523
#> 2026-01-01 106.1249
#> 2026-04-01 106.7744
#> 2026-07-01 107.4261
#> 2026-10-01 108.0620
#> 2027-01-01 108.7015
#> 2027-04-01 109.2162
#> 2027-07-01 109.7295
#> 2027-10-01 110.2233
#> 2028-01-01 110.8756
#> 2028-04-01 111.4005
#> 2028-07-01 111.9241
#> 2028-10-01 112.4277
# history continued with the growth of a projection on another level
proj <- make_xts(2020.1, val = 50 * 1.01^(0:63))
extend(cpi, path = proj, type = "chain")[p(2025.2, 2026.4)]
#>               value
#> 2025-04-01 103.8067
#> 2025-07-01 104.5852
#> 2025-10-01 105.6311
#> 2026-01-01 106.6874
#> 2026-04-01 107.7543
#> 2026-07-01 108.8318
#> 2026-10-01 109.9201
# a rate continued with the changes of a projection, which ends in 2028Q4
extend(ur, path = make_xts(2024.1, val = seq(4, 5, length.out = 20)),
  type = "chain", method = "diff")[p(2025.2, 2029.2)]
#>               value
#> 2025-04-01 4.200000
#> 2025-07-01 4.300000
#> 2025-10-01 4.352632
#> 2026-01-01 4.405263
#> 2026-04-01 4.457895
#> 2026-07-01 4.510526
#> 2026-10-01 4.563158
#> 2027-01-01 4.615789
#> 2027-04-01 4.668421
#> 2027-07-01 4.721053
#> 2027-10-01 4.773684
#> 2028-01-01 4.826316
#> 2028-04-01 4.878947
#> 2028-07-01 4.931579
#> 2028-10-01 4.984211
#> 2029-01-01       NA
#> 2029-04-01       NA
```
