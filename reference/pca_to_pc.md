# Convert annualized growth to quarterly growth

Convert annualized growth to quarterly growth

## Usage

``` r
pca_to_pc(x, freq = 4)
```

## Arguments

- x:

  ts-boxable object containing annualized growth (in percent)

- freq:

  numeric frequency of the time series e.g. 4 for quarterly

## Value

object of the same type as the input containing quarterly growth (in
percent)

## Examples

``` r
quarterly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_pca() |>
  pca_to_pc() |>
  tail()
#> # A tibble: 6 × 3
#>   id       time        value
#>   <chr>    <date>      <dbl>
#> 1 EGVLC_HI 2023-10-01 -0.178
#> 2 EGVLC_HI 2024-01-01  0.712
#> 3 EGVLC_HI 2024-04-01  0.353
#> 4 EGVLC_HI 2024-07-01  0    
#> 5 EGVLC_HI 2024-10-01  1.41 
#> 6 EGVLC_HI 2025-01-01  0.521
tsbox::ts_c(
  quarterly_data_example |>
    tsbox::ts_long() |>
    tsbox::ts_pca() |>
    pca_to_pc(),
  quarterly_data_example |>
    tsbox::ts_long() |>
    tsbox::ts_pc()
) |>
  dplyr::arrange(id, time) |>
  tsbox::ts_wide()
#> # A tibble: 109 × 61
#>    time       EAD_HI EAD_HI.1 EAFAC_HI EAFAC_HI.1 EAFFD_HI EAFFD_HI.1  EAF_HI
#>    <date>      <dbl>    <dbl>    <dbl>      <dbl>    <dbl>      <dbl>   <dbl>
#>  1 1998-01-01 NA       NA       NA         NA      NA         NA      NA     
#>  2 1998-04-01  0.460    0.460   -1.29      -1.29    1.18       1.18    0.0778
#>  3 1998-07-01  0.114    0.114   -1.50      -1.50    0.238      0.238  -0.529 
#>  4 1998-10-01 -0.457   -0.457    0.355      0.355   0.0966     0.0966  0.209 
#>  5 1999-01-01  1.49     1.49     0.267      0.267  -0.257     -0.257  -0.0278
#>  6 1999-04-01  1.81     1.81    -0.246     -0.246   1.64       1.64    0.814 
#>  7 1999-07-01  2.44     2.44    -1.92      -1.92    0.938      0.938  -0.302 
#>  8 1999-10-01  3.25     3.25     3.50       3.50    0.309      0.309   1.67  
#>  9 2000-01-01  0.840    0.840   -0.253     -0.253   0.835      0.835   0.362 
#> 10 2000-04-01  1.35     1.35     1.75       1.75    1.82       1.82    1.79  
#> # ℹ 99 more rows
#> # ℹ 53 more variables: EAF_HI.1 <dbl>, ECT_HI <dbl>, ECT_HI.1 <dbl>,
#> #   EED_HI <dbl>, EED_HI.1 <dbl>, EFI_HI <dbl>, EFI_HI.1 <dbl>, EGVFD_HI <dbl>,
#> #   EGVFD_HI.1 <dbl>, EGVLC_HI <dbl>, EGVLC_HI.1 <dbl>, EGVSTEDNS_HI <dbl>,
#> #   EGVSTEDNS_HI.1 <dbl>, EGVST_HI <dbl>, EGVST_HI.1 <dbl>, EGV_HI <dbl>,
#> #   EGV_HI.1 <dbl>, EHC_HI <dbl>, EHC_HI.1 <dbl>, EIF_HI <dbl>, EIF_HI.1 <dbl>,
#> #   EMA_HI <dbl>, EMA_HI.1 <dbl>, EMN_HI <dbl>, EMN_HI.1 <dbl>, EOS_HI <dbl>, …
```
