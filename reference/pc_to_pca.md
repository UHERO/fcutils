# Convert growth per period to annualized growth

Convert growth per period to annualized growth

## Usage

``` r
pc_to_pca(x, freq = 4)
```

## Arguments

- x:

  ts-boxable object containing growth per period (in percent)

- freq:

  periods per year: 4 (default) for quarterly, 12 for monthly. It is not
  read from x.

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
# the round trip gives tsbox's annualized growth
jobs <- conv_xts(quarterly_data_example)$E_NF_HI
tail(tsbox::ts_c(
  from_pc = pc_to_pca(tsbox::ts_pc(jobs)),
  pca = tsbox::ts_pca(jobs)
), 3)
#>             from_pc      pca
#> 2024-07-01 2.064634 2.064634
#> 2024-10-01 2.222258 2.222258
#> 2025-01-01 2.920348 2.920348
```
