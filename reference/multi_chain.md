# Extend "ts-boxable" objects by chaining (extension of `tsbox::ts_chain()`)

Extend "ts-boxable" objects by chaining (extension of
[`tsbox::ts_chain()`](https://docs.ropensci.org/tsbox/reference/ts_bind.html))

## Usage

``` r
multi_chain(
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
  which case all series common to x and y are extended)

- method:

  "ratio" (default) scales y so that it meets x, which keeps y's growth
  rates (for levels); "diff" shifts y by a constant, which keeps y's
  changes (for rates such as the unemployment rate)

- direction:

  "both" (default) extends x at both ends, "forward" only after its last
  observation, "backward" only before its first

- match:

  how series in x find their partner in y: "name" (default) needs the
  same name; "stem" compares names without a final frequency or solution
  ending (`_A`, `_Q`, `_M`, `_SOLA`, `_SOLQ`, `_SOLM`, or the same with
  `.`), so `UR_AT_US_A` pairs with `UR_AT_US_SOLQ`. A single series in x
  always pairs with a single series in y. The result keeps x's names.

- verbose:

  if TRUE (default), list the series being extended

## Value

object of the same type as the input with extended series

## Details

This function performs a similar operation to
[`tsbox::ts_chain()`](https://docs.ropensci.org/tsbox/reference/ts_bind.html),
but for multiple series. x and y are linked at x's first observation
(going backward) and its last (going forward); y needs a value at those
dates. The default, method "ratio" with direction "both", is
[`tsbox::ts_chain()`](https://docs.ropensci.org/tsbox/reference/ts_bind.html).

## Examples

``` r
quarterly_data_example |>
  dplyr::filter(time < as.Date("2000-01-01")) |>
  multi_chain(quarterly_data_example, ids = c("E_NF_HI", "ECT_HI"))
#> Extending the following series: 
#> E_NF_HI, ECT_HI
#> # A tibble: 109 × 31
#>    time       E_NF_HI ECT_HI EMN_HI EWT_HI ERT_HI E_TRADE_HI E_TU_HI ETWTANS_HI
#>    <date>       <dbl>  <dbl>  <dbl>  <dbl>  <dbl>      <dbl>   <dbl>      <dbl>
#>  1 1998-01-01    531.   23.3   15.9   15.9   65.2       81.0    27.9       9.97
#>  2 1998-04-01    531.   23.0   15.8   15.9   64.6       80.6    27.9      10.0 
#>  3 1998-07-01    533.   23.0   15.8   15.9   64.2       80.1    27.6      10.1 
#>  4 1998-10-01    531.   23.1   15.8   16     64.2       80.2    27.3      10   
#>  5 1999-01-01    530.   22.8   15.8   15.8   64.1       79.8    27.0       9.73
#>  6 1999-04-01    533.   22.9   15.9   15.8   64.8       80.6    27.4       9.97
#>  7 1999-07-01    537.   22.9   16.1   15.8   65.1       81.0    27.6      10.2 
#>  8 1999-10-01    541.   23.3   16.3   16.0   65.9       82.0    27.9      10.3 
#>  9 2000-01-01    544.   24.3   NA     NA     NA         NA      NA        NA   
#> 10 2000-04-01    551.   25.0   NA     NA     NA         NA      NA        NA   
#> # ℹ 99 more rows
#> # ℹ 22 more variables: ETWNS_HI <dbl>, EUT_HI <dbl>, EIF_HI <dbl>,
#> #   EFI_HI <dbl>, E_FIR_HI <dbl>, ERE_HI <dbl>, EPS_HI <dbl>, E_PBS_HI <dbl>,
#> #   E_ELSE_HI <dbl>, EMA_HI <dbl>, EAD_HI <dbl>, EED_HI <dbl>, EHC_HI <dbl>,
#> #   EAF_HI <dbl>, EAFAC_HI <dbl>, EAFFD_HI <dbl>, EOS_HI <dbl>, EGV_HI <dbl>,
#> #   EGVFD_HI <dbl>, EGVST_HI <dbl>, EGVSTEDNS_HI <dbl>, EGVLC_HI <dbl>
quarterly_data_example |>
  dplyr::filter(time < as.Date("2000-01-01")) |>
  multi_chain(quarterly_data_example)
#> Extending the following series: 
#> E_NF_HI, ECT_HI, EMN_HI, EWT_HI, ERT_HI, E_TRADE_HI, E_TU_HI, ETWTANS_HI, ETWNS_HI, EUT_HI, EIF_HI, EFI_HI, E_FIR_HI, ERE_HI, EPS_HI, E_PBS_HI, E_ELSE_HI, EMA_HI, EAD_HI, EED_HI, EHC_HI, EAF_HI, EAFAC_HI, EAFFD_HI, EOS_HI, EGV_HI, EGVFD_HI, EGVST_HI, EGVSTEDNS_HI, EGVLC_HI
#> # A tibble: 109 × 31
#>    time       E_NF_HI ECT_HI EMN_HI EWT_HI ERT_HI E_TRADE_HI E_TU_HI ETWTANS_HI
#>    <date>       <dbl>  <dbl>  <dbl>  <dbl>  <dbl>      <dbl>   <dbl>      <dbl>
#>  1 1998-01-01    531.   23.3   15.9   15.9   65.2       81.0    27.9       9.97
#>  2 1998-04-01    531.   23.0   15.8   15.9   64.6       80.6    27.9      10.0 
#>  3 1998-07-01    533.   23.0   15.8   15.9   64.2       80.1    27.6      10.1 
#>  4 1998-10-01    531.   23.1   15.8   16     64.2       80.2    27.3      10   
#>  5 1999-01-01    530.   22.8   15.8   15.8   64.1       79.8    27.0       9.73
#>  6 1999-04-01    533.   22.9   15.9   15.8   64.8       80.6    27.4       9.97
#>  7 1999-07-01    537.   22.9   16.1   15.8   65.1       81.0    27.6      10.2 
#>  8 1999-10-01    541.   23.3   16.3   16.0   65.9       82.0    27.9      10.3 
#>  9 2000-01-01    544.   24.3   16.3   16.2   66.0       82.3    28.0      10.4 
#> 10 2000-04-01    551.   25.0   16.4   16.2   66.0       82.3    28.1      10.6 
#> # ℹ 99 more rows
#> # ℹ 22 more variables: ETWNS_HI <dbl>, EUT_HI <dbl>, EIF_HI <dbl>,
#> #   EFI_HI <dbl>, E_FIR_HI <dbl>, ERE_HI <dbl>, EPS_HI <dbl>, E_PBS_HI <dbl>,
#> #   E_ELSE_HI <dbl>, EMA_HI <dbl>, EAD_HI <dbl>, EED_HI <dbl>, EHC_HI <dbl>,
#> #   EAF_HI <dbl>, EAFAC_HI <dbl>, EAFFD_HI <dbl>, EOS_HI <dbl>, EGV_HI <dbl>,
#> #   EGVFD_HI <dbl>, EGVST_HI <dbl>, EGVSTEDNS_HI <dbl>, EGVLC_HI <dbl>
# continue an annual history with the changes of a forecast
hist <- make_xts(2020, val = c(8.1, 5.4, 3.6, 3.6))
fcst <- make_xts(2022, val = c(3.5, 3.7, 4.0, 4.2, 4.3))
multi_chain(hist, fcst, method = "diff", direction = "forward")
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
fcst <- make_xts(2022, val = tibble::tibble(UR_SOLA = c(3.6, 3.8, 4.1), LF_SOLA = c(165, 166, 168)))
multi_chain(hist, fcst, match = "stem", direction = "forward")
#> Extending the following series: 
#> UR_A, LF_A
#>            UR_A     LF_A
#> 2020-01-01  8.1 160.0000
#> 2021-01-01  5.4 161.0000
#> 2022-01-01  3.6 164.0000
#> 2023-01-01  3.8 164.9939
#> 2024-01-01  4.1 166.9818
```
