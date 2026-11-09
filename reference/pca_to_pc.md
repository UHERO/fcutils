# Convert annualized growth to growth per period

Convert annualized growth to growth per period

## Usage

``` r
pca_to_pc(x, freq = 4)
```

## Arguments

- x:

  ts-boxable object containing annualized growth (in percent)

- freq:

  periods per year: 4 (default) for quarterly, 12 for monthly. It is not
  read from x.

## Value

object of the same type as the input containing growth per period (in
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
# the round trip gives tsbox's growth per period
jobs <- conv_xts(quarterly_data_example)$E_NF_HI
tail(tsbox::ts_c(
  from_pca = pca_to_pc(tsbox::ts_pca(jobs)),
  pc = tsbox::ts_pc(jobs)
), 3)
#>             from_pca        pc
#> 2024-07-01 0.5122097 0.5122097
#> 2024-10-01 0.5509937 0.5509937
#> 2025-01-01 0.7222250 0.7222250
```
