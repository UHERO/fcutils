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

object of the same type as the input containing fiscal year to date
growth rate

## Details

Works as ytd_gr(), with the fiscal year starting in July: the percent
change of the fiscal-year-to-date average from the same date a year
earlier. An NA makes the rest of its fiscal year NA.

## Examples

``` r
quarterly_data_example |>
  fytd_gr() |>
  tail()
#> # A tibble: 6 × 31
#>   time       E_NF_HI ECT_HI   EMN_HI  EWT_HI ERT_HI E_TRADE_HI E_TU_HI
#>   <date>       <dbl>  <dbl>    <dbl>   <dbl>  <dbl>      <dbl>   <dbl>
#> 1 2023-10-01   1.08    1.89 -0.201   -0.0960 -0.102     -0.113   1.06 
#> 2 2024-01-01   1.02    1.96 -0.0555  -0.128  -0.493     -0.426   0.241
#> 3 2024-04-01   0.869   2.18 -0.00436 -0.0959 -0.765     -0.625  -0.293
#> 4 2024-07-01   1.19    4.08  1.47     0      -1.58      -1.22    1.42 
#> 5 2024-10-01   1.50    3.16  1.74    -0.0961 -0.640     -0.500   1.57 
#> 6 2025-01-01   1.58    2.55  2.11    -0.320  -0.239     -0.213   1.76 
#> # ℹ 23 more variables: ETWTANS_HI <dbl>, ETWNS_HI <dbl>, EUT_HI <dbl>,
#> #   EIF_HI <dbl>, EFI_HI <dbl>, E_FIR_HI <dbl>, ERE_HI <dbl>, EPS_HI <dbl>,
#> #   E_PBS_HI <dbl>, E_ELSE_HI <dbl>, EMA_HI <dbl>, EAD_HI <dbl>, EED_HI <dbl>,
#> #   EHC_HI <dbl>, EAF_HI <dbl>, EAFAC_HI <dbl>, EAFFD_HI <dbl>, EOS_HI <dbl>,
#> #   EGV_HI <dbl>, EGVFD_HI <dbl>, EGVST_HI <dbl>, EGVSTEDNS_HI <dbl>,
#> #   EGVLC_HI <dbl>
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
