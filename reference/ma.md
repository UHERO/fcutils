# Backward looking moving average

Backward looking moving average

## Usage

``` r
ma(x, order)
```

## Arguments

- x:

  ts-boxable object

- order:

  numeric order (window length) of moving average, includes
  contemporaneous observation

## Value

object of the same type as the input containing moving average

## Examples

``` r
quarterly_data_example |>
  ma(4) |>
  head()
#> # A tibble: 6 × 31
#>   time       E_NF_HI ECT_HI EMN_HI EWT_HI ERT_HI E_TRADE_HI E_TU_HI ETWTANS_HI
#>   <date>       <dbl>  <dbl>  <dbl>  <dbl>  <dbl>      <dbl>   <dbl>      <dbl>
#> 1 1998-01-01     NA    NA     NA     NA     NA         NA      NA        NA   
#> 2 1998-04-01     NA    NA     NA     NA     NA         NA      NA        NA   
#> 3 1998-07-01     NA    NA     NA     NA     NA         NA      NA        NA   
#> 4 1998-10-01    531.   23.1   15.8   15.9   64.6       80.5    27.7      10.0 
#> 5 1999-01-01    531.   23.0   15.8   15.9   64.3       80.2    27.5       9.98
#> 6 1999-04-01    532.   22.9   15.8   15.9   64.3       80.2    27.3       9.96
#> # ℹ 22 more variables: ETWNS_HI <dbl>, EUT_HI <dbl>, EIF_HI <dbl>,
#> #   EFI_HI <dbl>, E_FIR_HI <dbl>, ERE_HI <dbl>, EPS_HI <dbl>, E_PBS_HI <dbl>,
#> #   E_ELSE_HI <dbl>, EMA_HI <dbl>, EAD_HI <dbl>, EED_HI <dbl>, EHC_HI <dbl>,
#> #   EAF_HI <dbl>, EAFAC_HI <dbl>, EAFFD_HI <dbl>, EOS_HI <dbl>, EGV_HI <dbl>,
#> #   EGVFD_HI <dbl>, EGVST_HI <dbl>, EGVSTEDNS_HI <dbl>, EGVLC_HI <dbl>
```
