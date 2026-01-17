# Convert "ts-boxable" objects to wide format (extension of `tsbox::ts_wide()`)

Convert "ts-boxable" objects to wide format (extension of
[`tsbox::ts_wide()`](https://docs.ropensci.org/tsbox/reference/ts_long.html))

## Usage

``` r
conv_wide(x)
```

## Arguments

- x:

  a "ts-boxable" object to be converted

## Value

returns an object in wide format with a `time` column and series values
in subsequent columns with `id` in column heading.

## Details

This function performs a similar operation to
[`tsbox::ts_wide()`](https://docs.ropensci.org/tsbox/reference/ts_long.html).
It converts ts-boxable objects to the wide format. An important
difference compared with
[`tsbox::ts_wide()`](https://docs.ropensci.org/tsbox/reference/ts_long.html)
is that `conv_wide()` does not require x to be a long tbl.

## Examples

``` r
quarterly_data_example |>
  conv_tslist() |>
  conv_wide()
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
quarterly_data_example |>
  conv_xts() |>
  conv_wide()
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
quarterly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_pick("E_NF_HI") |>
  tsbox::ts_xts() |>
  conv_wide()
#> # A tibble: 109 × 2
#>    time       E_NF_HI
#>    <date>       <dbl>
#>  1 1998-01-01    531.
#>  2 1998-04-01    531.
#>  3 1998-07-01    533.
#>  4 1998-10-01    531.
#>  5 1999-01-01    530.
#>  6 1999-04-01    533.
#>  7 1999-07-01    537.
#>  8 1999-10-01    541.
#>  9 2000-01-01    544.
#> 10 2000-04-01    551.
#> # ℹ 99 more rows
```
