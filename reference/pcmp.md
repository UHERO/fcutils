# Calculate multi-period average growth

Calculate multi-period average growth

## Usage

``` r
pcmp(x, lag = 4, comp_freq = 1)
```

## Arguments

- x:

  ts-boxable object for which growth is calculated (in levels)

- lag:

  number of periods over which growth is calculated

- comp_freq:

  compounding frequency (1 if period by period, 4 if annualized for
  quarterly data, etc.)

## Value

object of the same type as the input ontaining the average growth of x
(in percent)

## Examples

``` r
quarterly_data_example |>
  pcmp(20) |>
  tail()
#> # A tibble: 6 × 31
#>   time       E_NF_HI ECT_HI EMN_HI EWT_HI ERT_HI E_TRADE_HI E_TU_HI ETWTANS_HI
#>   <date>       <dbl>  <dbl>  <dbl>  <dbl>  <dbl>      <dbl>   <dbl>      <dbl>
#> 1 2023-10-01 -0.218  0.0387 -0.581 -0.261 -0.558     -0.497  0.0386     0.101 
#> 2 2024-01-01 -0.163  0.147  -0.543 -0.306 -0.531     -0.484  0.0383    -0.0336
#> 3 2024-04-01 -0.161  0.216  -0.524 -0.261 -0.509     -0.456 -0.221     -0.101 
#> 4 2024-07-01 -0.146  0.254  -0.438 -0.253 -0.509     -0.457 -0.222     -0.217 
#> 5 2024-10-01 -0.130  0.188  -0.333 -0.253 -0.436     -0.396 -0.257     -0.283 
#> 6 2025-01-01 -0.0973 0.266  -0.257 -0.263 -0.424     -0.386 -0.223     -0.381 
#> # ℹ 22 more variables: ETWNS_HI <dbl>, EUT_HI <dbl>, EIF_HI <dbl>,
#> #   EFI_HI <dbl>, E_FIR_HI <dbl>, ERE_HI <dbl>, EPS_HI <dbl>, E_PBS_HI <dbl>,
#> #   E_ELSE_HI <dbl>, EMA_HI <dbl>, EAD_HI <dbl>, EED_HI <dbl>, EHC_HI <dbl>,
#> #   EAF_HI <dbl>, EAFAC_HI <dbl>, EAFFD_HI <dbl>, EOS_HI <dbl>, EGV_HI <dbl>,
#> #   EGVFD_HI <dbl>, EGVST_HI <dbl>, EGVSTEDNS_HI <dbl>, EGVLC_HI <dbl>
quarterly_data_example |>
  pcmp(4, 4) |>
  tail()
#> # A tibble: 6 × 31
#>   time       E_NF_HI ECT_HI EMN_HI EWT_HI ERT_HI E_TRADE_HI E_TU_HI ETWTANS_HI
#>   <date>       <dbl>  <dbl>  <dbl>  <dbl>  <dbl>      <dbl>   <dbl>      <dbl>
#> 1 2023-10-01   0.671   1.74 -0.609  0     -0.614     -0.500   0.456       4.17
#> 2 2024-01-01   0.893   2.09  0.235 -0.191 -1.27      -1.05   -1.36        1.71
#> 3 2024-04-01   0.428   2.85  0.149  0     -1.58      -1.22   -1.86        0   
#> 4 2024-07-01   1.19    4.08  1.47   0     -1.58      -1.22    1.42       -1.01
#> 5 2024-10-01   1.81    2.24  2.01  -0.192  0.309      0.230   1.73       -2.67
#> 6 2025-01-01   1.73    1.37  2.85  -0.766  0.567      0.364   2.14       -2.36
#> # ℹ 22 more variables: ETWNS_HI <dbl>, EUT_HI <dbl>, EIF_HI <dbl>,
#> #   EFI_HI <dbl>, E_FIR_HI <dbl>, ERE_HI <dbl>, EPS_HI <dbl>, E_PBS_HI <dbl>,
#> #   E_ELSE_HI <dbl>, EMA_HI <dbl>, EAD_HI <dbl>, EED_HI <dbl>, EHC_HI <dbl>,
#> #   EAF_HI <dbl>, EAFAC_HI <dbl>, EAFFD_HI <dbl>, EOS_HI <dbl>, EGV_HI <dbl>,
#> #   EGVFD_HI <dbl>, EGVST_HI <dbl>, EGVSTEDNS_HI <dbl>, EGVLC_HI <dbl>
quarterly_data_example |>
  pcmp(1, 4) |>
  tail()
#> # A tibble: 6 × 31
#>   time       E_NF_HI ECT_HI EMN_HI EWT_HI ERT_HI E_TRADE_HI E_TU_HI ETWTANS_HI
#>   <date>       <dbl>  <dbl>  <dbl>  <dbl>  <dbl>      <dbl>   <dbl>      <dbl>
#> 1 2023-10-01  -0.257   2.54   2.30  0.771 -4.60     -3.47    -2.62        5.52
#> 2 2024-01-01   3.26    6.54   2.44  0.770 -0.206     0.0532   0.916      -3.94
#> 3 2024-04-01  -0.269   3.08  -1.13 -0.764 -0.822    -0.666    0.734      -3.98
#> 4 2024-07-01   2.06    4.22   2.30 -0.766 -0.618    -0.779    6.88       -1.35
#> 5 2024-10-01   2.22   -4.55   4.50  0      2.93      2.34    -1.42       -1.36
#> 6 2025-01-01   2.92    2.98   5.87 -1.53   0.824     0.589    2.57       -2.71
#> # ℹ 22 more variables: ETWNS_HI <dbl>, EUT_HI <dbl>, EIF_HI <dbl>,
#> #   EFI_HI <dbl>, E_FIR_HI <dbl>, ERE_HI <dbl>, EPS_HI <dbl>, E_PBS_HI <dbl>,
#> #   E_ELSE_HI <dbl>, EMA_HI <dbl>, EAD_HI <dbl>, EED_HI <dbl>, EHC_HI <dbl>,
#> #   EAF_HI <dbl>, EAFAC_HI <dbl>, EAFFD_HI <dbl>, EOS_HI <dbl>, EGV_HI <dbl>,
#> #   EGVFD_HI <dbl>, EGVST_HI <dbl>, EGVSTEDNS_HI <dbl>, EGVLC_HI <dbl>
```
