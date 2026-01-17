# Get indexed series (wrapper around tsbox::ts_index())

Get indexed series (wrapper around tsbox::ts_index())

## Usage

``` r
index(x, base_per = as.character(Sys.Date()), base_value = 100)
```

## Arguments

- x:

  ts-boxable object to be indexed

- base_per:

  base date when the index is set to base_value (see examples). If two
  dates are provided, the mean in the range is set equal to base_value.

- base_value:

  numeric value of the index at base_per (e.g. 1 or 100)

## Value

indexed object of the same type as the input

## Examples

``` r
quarterly_data_example |>
  index(2010.1)
#> # A tibble: 109 × 31
#>    time       E_NF_HI ECT_HI EMN_HI EWT_HI ERT_HI E_TRADE_HI E_TU_HI ETWTANS_HI
#>    <date>       <dbl>  <dbl>  <dbl>  <dbl>  <dbl>      <dbl>   <dbl>      <dbl>
#>  1 1998-01-01    90.5   76.6   119.   88.5   98.5       96.3    106.       142.
#>  2 1998-04-01    90.6   75.7   118.   88.8   97.7       95.8    106.       143.
#>  3 1998-07-01    90.9   75.7   118.   88.7   97.1       95.3    105.       145.
#>  4 1998-10-01    90.7   75.9   118.   89.2   97.0       95.4    104.       143.
#>  5 1999-01-01    90.3   75.0   118.   87.9   96.8       94.9    102.       139.
#>  6 1999-04-01    91.0   75.3   119.   88.3   97.9       95.9    104.       142.
#>  7 1999-07-01    91.7   75.5   120.   88.3   98.4       96.3    105.       145.
#>  8 1999-10-01    92.3   76.7   121.   89.4   99.6       97.5    106.       147.
#>  9 2000-01-01    92.9   79.9   121.   90.5   99.8       97.8    106.       149.
#> 10 2000-04-01    94.0   82.3   123.   90.5   99.8       97.8    107.       151.
#> # ℹ 99 more rows
#> # ℹ 22 more variables: ETWNS_HI <dbl>, EUT_HI <dbl>, EIF_HI <dbl>,
#> #   EFI_HI <dbl>, E_FIR_HI <dbl>, ERE_HI <dbl>, EPS_HI <dbl>, E_PBS_HI <dbl>,
#> #   E_ELSE_HI <dbl>, EMA_HI <dbl>, EAD_HI <dbl>, EED_HI <dbl>, EHC_HI <dbl>,
#> #   EAF_HI <dbl>, EAFAC_HI <dbl>, EAFFD_HI <dbl>, EOS_HI <dbl>, EGV_HI <dbl>,
#> #   EGVFD_HI <dbl>, EGVST_HI <dbl>, EGVSTEDNS_HI <dbl>, EGVLC_HI <dbl>
quarterly_data_example |>
  index(c(2010.1, 2010.4))
#> # A tibble: 109 × 31
#>    time       E_NF_HI ECT_HI EMN_HI EWT_HI ERT_HI E_TRADE_HI E_TU_HI ETWTANS_HI
#>    <date>       <dbl>  <dbl>  <dbl>  <dbl>  <dbl>      <dbl>   <dbl>      <dbl>
#>  1 1998-01-01    90.2   78.1   120.   88.5   98.3       96.2    104.       141.
#>  2 1998-04-01    90.3   77.2   119.   88.9   97.5       95.6    104.       141.
#>  3 1998-07-01    90.6   77.2   119.   88.7   96.9       95.1    103.       143.
#>  4 1998-10-01    90.4   77.4   119.   89.3   96.8       95.2    102.       141.
#>  5 1999-01-01    90.0   76.5   119.   88.0   96.6       94.8    101.       137.
#>  6 1999-04-01    90.7   76.7   120.   88.3   97.7       95.7    103.       141.
#>  7 1999-07-01    91.4   77.0   122.   88.3   98.2       96.1    103.       143.
#>  8 1999-10-01    92.0   78.2   123.   89.4   99.4       97.3    105.       145.
#>  9 2000-01-01    92.6   81.4   123.   90.6   99.6       97.6    105.       147.
#> 10 2000-04-01    93.7   83.9   124.   90.6   99.6       97.7    105.       149.
#> # ℹ 99 more rows
#> # ℹ 22 more variables: ETWNS_HI <dbl>, EUT_HI <dbl>, EIF_HI <dbl>,
#> #   EFI_HI <dbl>, E_FIR_HI <dbl>, ERE_HI <dbl>, EPS_HI <dbl>, E_PBS_HI <dbl>,
#> #   E_ELSE_HI <dbl>, EMA_HI <dbl>, EAD_HI <dbl>, EED_HI <dbl>, EHC_HI <dbl>,
#> #   EAF_HI <dbl>, EAFAC_HI <dbl>, EAFFD_HI <dbl>, EOS_HI <dbl>, EGV_HI <dbl>,
#> #   EGVFD_HI <dbl>, EGVST_HI <dbl>, EGVSTEDNS_HI <dbl>, EGVLC_HI <dbl>
quarterly_data_example |>
  index(c("2010-01-01", "2010-12-31"), 1)
#> # A tibble: 109 × 31
#>    time       E_NF_HI ECT_HI EMN_HI EWT_HI ERT_HI E_TRADE_HI E_TU_HI ETWTANS_HI
#>    <date>       <dbl>  <dbl>  <dbl>  <dbl>  <dbl>      <dbl>   <dbl>      <dbl>
#>  1 1998-01-01   0.902  0.781   1.20  0.885  0.983      0.962    1.04       1.41
#>  2 1998-04-01   0.903  0.772   1.19  0.889  0.975      0.956    1.04       1.41
#>  3 1998-07-01   0.906  0.772   1.19  0.887  0.969      0.951    1.03       1.43
#>  4 1998-10-01   0.904  0.774   1.19  0.893  0.968      0.952    1.02       1.41
#>  5 1999-01-01   0.900  0.765   1.19  0.880  0.966      0.948    1.01       1.37
#>  6 1999-04-01   0.907  0.767   1.20  0.883  0.977      0.957    1.03       1.41
#>  7 1999-07-01   0.914  0.770   1.22  0.883  0.982      0.961    1.03       1.43
#>  8 1999-10-01   0.920  0.782   1.23  0.894  0.994      0.973    1.05       1.45
#>  9 2000-01-01   0.926  0.814   1.23  0.906  0.996      0.976    1.05       1.47
#> 10 2000-04-01   0.937  0.839   1.24  0.906  0.996      0.977    1.05       1.49
#> # ℹ 99 more rows
#> # ℹ 22 more variables: ETWNS_HI <dbl>, EUT_HI <dbl>, EIF_HI <dbl>,
#> #   EFI_HI <dbl>, E_FIR_HI <dbl>, ERE_HI <dbl>, EPS_HI <dbl>, E_PBS_HI <dbl>,
#> #   E_ELSE_HI <dbl>, EMA_HI <dbl>, EAD_HI <dbl>, EED_HI <dbl>, EHC_HI <dbl>,
#> #   EAF_HI <dbl>, EAFAC_HI <dbl>, EAFFD_HI <dbl>, EOS_HI <dbl>, EGV_HI <dbl>,
#> #   EGVFD_HI <dbl>, EGVST_HI <dbl>, EGVSTEDNS_HI <dbl>, EGVLC_HI <dbl>
```
