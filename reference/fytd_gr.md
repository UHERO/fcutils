# Fiscal year to date growth rate

Fiscal year to date growth rate

## Usage

``` r
fytd_gr(x)
```

## Arguments

- x:

  a ts-boxable object

## Value

object of the same type as the input containing year to date growth rate

## Details

this function operates similarly to ytd_gr() but assumes that the fiscal
year starts in July and accumulates the values from that month onward.

## Examples

``` r
quarterly_data_example |>
  fytd_gr()
#> # A tibble: 109 × 31
#>    time       E_NF_HI ECT_HI  EMN_HI EWT_HI ERT_HI E_TRADE_HI E_TU_HI ETWTANS_HI
#>    <date>       <dbl>  <dbl>   <dbl>  <dbl>  <dbl>      <dbl>   <dbl>      <dbl>
#>  1 1998-01-01  NA     NA     NA      NA     NA         NA      NA         NA    
#>  2 1998-04-01  NA     NA     NA      NA     NA         NA      NA         NA    
#>  3 1998-07-01  NA     NA     NA      NA     NA         NA      NA         NA    
#>  4 1998-10-01  NA     NA     NA      NA     NA         NA      NA         NA    
#>  5 1999-01-01   0.136 -1.33  -0.694   0.140 -1.53      -1.19   -2.07      -0.111
#>  6 1999-04-01   0.163 -0.864 -0.0497 -0.157 -0.886     -0.732  -1.91      -0.417
#>  7 1999-07-01   0.871 -0.319  2.07   -0.419  1.40       1.03   -0.145      0.329
#>  8 1999-10-01   1.35   0.396  2.58   -0.104  2.05       1.61    1.00       1.49 
#>  9 2000-01-01   1.84   2.39   2.78    0.909  2.39       2.09    1.81       3.24 
#> 10 2000-04-01   2.22   4.13   2.88    1.31   2.27       2.07    2.00       3.93 
#> # ℹ 99 more rows
#> # ℹ 22 more variables: ETWNS_HI <dbl>, EUT_HI <dbl>, EIF_HI <dbl>,
#> #   EFI_HI <dbl>, E_FIR_HI <dbl>, ERE_HI <dbl>, EPS_HI <dbl>, E_PBS_HI <dbl>,
#> #   E_ELSE_HI <dbl>, EMA_HI <dbl>, EAD_HI <dbl>, EED_HI <dbl>, EHC_HI <dbl>,
#> #   EAF_HI <dbl>, EAFAC_HI <dbl>, EAFFD_HI <dbl>, EOS_HI <dbl>, EGV_HI <dbl>,
#> #   EGVFD_HI <dbl>, EGVST_HI <dbl>, EGVSTEDNS_HI <dbl>, EGVLC_HI <dbl>
monthly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_pick("VISNS_HI") |>
  tsbox::ts_xts() |>
  fytd_gr() |>
  tail()
#>            VISNS_HI
#> 2024-10-01 4.244738
#> 2024-11-01 4.442723
#> 2024-12-01 4.640796
#> 2025-01-01 4.517318
#> 2025-02-01 3.751294
#> 2025-03-01 3.654328
```
