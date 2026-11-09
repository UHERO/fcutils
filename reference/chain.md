# Extend "ts-boxable" objects by chaining (extension of `tsbox::ts_chain()`)

Extend "ts-boxable" objects by chaining (extension of
[`tsbox::ts_chain()`](https://docs.ropensci.org/tsbox/reference/ts_bind.html))

## Usage

``` r
chain(
  x,
  y,
  ids = NULL,
  method = c("ratio", "diff"),
  direction = c("both", "forward", "backward"),
  match = c("name", "stem"),
  verbose = TRUE
)
```

## Arguments

- x:

  a "ts-boxable" object to be extended

- y:

  a "ts-boxable" object containing data to extend x

- ids:

  a character vector with series names to be extended (default: NULL, in
  which case all series common to x and y are extended). Names not in
  both x and y are skipped.

- method:

  "ratio" (default) scales y so that it meets x, which keeps y's growth
  rates (for levels); "diff" shifts y by a constant, which keeps y's
  changes (for rates such as the unemployment rate). Either one method
  for all series, or a named vector giving the method by series name in
  x, `c(UR_AT_US = "diff", FFR_AT_US = "diff")`; series not named use
  "ratio".

- direction:

  "both" (default) extends x at both ends, "forward" only after its last
  observation, "backward" only before its first

- match:

  how series in x find their partner in y: "name" (default) needs the
  same name; "stem" compares names without a final frequency or solution
  ending (`_A`, `_Q`, `_M`, `_SOLA`, `_SOLQ`, `_SOLM`, or the same with
  `.`), so `UR_AT_US_Q` pairs with `UR_AT_US_SOLQ`. A single series in x
  always pairs with a single series in y. The result keeps x's names. x
  and y must have the same frequency; chain() does not convert or check
  it.

- verbose:

  if TRUE (default), list the series being extended

## Value

object of the same type as x with extended series. Series in y with no
partner in x are dropped.

## Details

chain() joins data: two sources of the same series, or two banks. To
forecast one series from assumptions or a path, use
[`extend()`](https://uhero.github.io/fcutils/reference/extend.md); see
"Which function" there.

This function performs a similar operation to
[`tsbox::ts_chain()`](https://docs.ropensci.org/tsbox/reference/ts_bind.html),
but for multiple series. x and y are linked at x's first observation
(going backward) and its last (going forward). With `method = "diff"` or
a single direction, y needs a value at those dates, or chain() stops.
The default, method "ratio" with direction "both", is
[`tsbox::ts_chain()`](https://docs.ropensci.org/tsbox/reference/ts_bind.html):
if y lacks a value at those dates, it links at the nearest date where
both have one.

## Examples

``` r
# extend two of the series; the others stay as they were
quarterly_data_example |>
  dplyr::filter(time < as.Date("2000-01-01")) |>
  chain(quarterly_data_example, ids = c("E_NF_HI", "ECT_HI")) |>
  dplyr::select(time, E_NF_HI, ECT_HI, EMN_HI) |>
  tail(3)
#> Extending the following series: 
#> E_NF_HI, ECT_HI
#> # A tibble: 3 × 4
#>   time       E_NF_HI ECT_HI EMN_HI
#>   <date>       <dbl>  <dbl>  <dbl>
#> 1 2024-07-01    640.   39.6     NA
#> 2 2024-10-01    643.   39.1     NA
#> 3 2025-01-01    648.   39.4     NA
# extend all of them
quarterly_data_example |>
  dplyr::filter(time < as.Date("2000-01-01")) |>
  chain(quarterly_data_example, verbose = FALSE) |>
  dplyr::select(time, E_NF_HI, ECT_HI, EMN_HI) |>
  tail(3)
#> # A tibble: 3 × 4
#>   time       E_NF_HI ECT_HI EMN_HI
#>   <date>       <dbl>  <dbl>  <dbl>
#> 1 2024-07-01    640.   39.6   12.8
#> 2 2024-10-01    643.   39.1   13.0
#> 3 2025-01-01    648.   39.4   13.2
# continue an annual history with the changes of a forecast
hist <- make_xts(2020, val = c(8.1, 5.4, 3.6, 3.6))
fcst <- make_xts(2022, val = c(3.5, 3.7, 4.0, 4.2, 4.3))
chain(hist, fcst, method = "diff", direction = "forward")
#> Extending the following series: 
#> value
#>            value
#> 2020-01-01   8.1
#> 2021-01-01   5.4
#> 2022-01-01   3.6
#> 2023-01-01   3.6
#> 2024-01-01   3.9
#> 2025-01-01   4.1
#> 2026-01-01   4.2
# pair series by name stem: UR_A with UR_SOLA, LF_A with LF_SOLA
hist <- make_xts(2020, val = tibble::tibble(UR_A = c(8.1, 5.4, 3.6), LF_A = c(160, 161, 164)))
fcst <- make_xts(2022, val = tibble::tibble(UR_SOLA = c(4.0, 4.4, 4.9), LF_SOLA = c(165, 166, 168)))
chain(hist, fcst, match = "stem", direction = "forward")
#> Extending the following series: 
#> UR_A, LF_A
#>            UR_A     LF_A
#> 2020-01-01 8.10 160.0000
#> 2021-01-01 5.40 161.0000
#> 2022-01-01 3.60 164.0000
#> 2023-01-01 3.96 164.9939
#> 2024-01-01 4.41 166.9818
# a method per series: changes for the rate (3.6 + 0.4, + 0.5), growth for
# the level
chain(hist, fcst, match = "stem", direction = "forward", method = c(UR_A = "diff"))
#> Extending the following series: 
#> UR_A, LF_A
#>            UR_A     LF_A
#> 2020-01-01  8.1 160.0000
#> 2021-01-01  5.4 161.0000
#> 2022-01-01  3.6 164.0000
#> 2023-01-01  4.0 164.9939
#> 2024-01-01  4.5 166.9818
```
