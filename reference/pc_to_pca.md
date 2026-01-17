# Convert quarterly growth to annualized growth

Convert quarterly growth to annualized growth

## Usage

``` r
pc_to_pca(x, freq = 4)
```

## Arguments

- x:

  ts-boxable object containing quarterly growth (in percent)

- freq:

  numeric frequency of the time series e.g. 4 for quarterly

## Value

object of the same type as the input containing annualized growth (in
percent)

## Examples

``` r
quarterly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_pc() |>
  pc_to_pca() |>
  tail()
#> # A tibble: 6 × 3
#>   id       time        value
#>   <chr>    <date>      <dbl>
#> 1 EGVLC_HI 2023-10-01 -0.709
#> 2 EGVLC_HI 2024-01-01  2.88 
#> 3 EGVLC_HI 2024-04-01  1.42 
#> 4 EGVLC_HI 2024-07-01  0    
#> 5 EGVLC_HI 2024-10-01  5.75 
#> 6 EGVLC_HI 2025-01-01  2.10 
tsbox::ts_c(
  quarterly_data_example |>
    tsbox::ts_long() |>
    tsbox::ts_pc() |>
    pc_to_pca(),
  quarterly_data_example |>
    tsbox::ts_long() |>
    tsbox::ts_pca()
) |>
  dplyr::arrange(id, time) |>
  tsbox::ts_wide()
#> # A tibble: 109 × 61
#>    time       EAD_HI EAD_HI.1 EAFAC_HI EAFAC_HI.1 EAFFD_HI EAFFD_HI.1 EAF_HI
#>    <date>      <dbl>    <dbl>    <dbl>      <dbl>    <dbl>      <dbl>  <dbl>
#>  1 1998-01-01 NA       NA       NA         NA       NA         NA     NA    
#>  2 1998-04-01  1.85     1.85    -5.06      -5.06     4.82       4.82   0.312
#>  3 1998-07-01  0.458    0.458   -5.87      -5.87     0.955      0.955 -2.10 
#>  4 1998-10-01 -1.82    -1.82     1.43       1.43     0.387      0.387  0.840
#>  5 1999-01-01  6.11     6.11     1.07       1.07    -1.02      -1.02  -0.111
#>  6 1999-04-01  7.44     7.44    -0.978     -0.978    6.73       6.73   3.30 
#>  7 1999-07-01 10.1     10.1     -7.46      -7.46     3.81       3.81  -1.20 
#>  8 1999-10-01 13.7     13.7     14.7       14.7      1.24       1.24   6.85 
#>  9 2000-01-01  3.40     3.40    -1.01      -1.01     3.38       3.38   1.46 
#> 10 2000-04-01  5.53     5.53     7.19       7.19     7.48       7.48   7.35 
#> # ℹ 99 more rows
#> # ℹ 53 more variables: EAF_HI.1 <dbl>, ECT_HI <dbl>, ECT_HI.1 <dbl>,
#> #   EED_HI <dbl>, EED_HI.1 <dbl>, EFI_HI <dbl>, EFI_HI.1 <dbl>, EGVFD_HI <dbl>,
#> #   EGVFD_HI.1 <dbl>, EGVLC_HI <dbl>, EGVLC_HI.1 <dbl>, EGVSTEDNS_HI <dbl>,
#> #   EGVSTEDNS_HI.1 <dbl>, EGVST_HI <dbl>, EGVST_HI.1 <dbl>, EGV_HI <dbl>,
#> #   EGV_HI.1 <dbl>, EHC_HI <dbl>, EHC_HI.1 <dbl>, EIF_HI <dbl>, EIF_HI.1 <dbl>,
#> #   EMA_HI <dbl>, EMA_HI.1 <dbl>, EMN_HI <dbl>, EMN_HI.1 <dbl>, EOS_HI <dbl>, …
```
