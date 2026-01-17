# Extend "ts-boxable" objects by chaining (extension of `tsbox::ts_chain()`)

Extend "ts-boxable" objects by chaining (extension of
[`tsbox::ts_chain()`](https://docs.ropensci.org/tsbox/reference/ts_bind.html))

## Usage

``` r
multi_chain(x, y, ids = NULL)
```

## Arguments

- x:

  a "ts-boxable" object to be extended

- y:

  a "ts-boxable" object containing data to extend x

- ids:

  a character vector with series names to be extended (default: NULL, in
  which case all series common to x and y are extended)

## Value

object of the same type as the input with extended series

## Details

This function performs a similar operation to
[`tsbox::ts_chain()`](https://docs.ropensci.org/tsbox/reference/ts_bind.html),
but for multiple series.

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
```
