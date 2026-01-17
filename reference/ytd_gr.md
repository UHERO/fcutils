# Year to date growth rate

Year to date growth rate

## Usage

``` r
ytd_gr(x)
```

## Arguments

- x:

  a ts-boxable object

## Value

object of the same type as the input containing year to date growth rate

## Examples

``` r
monthly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_pick("VISNS_HI") |>
  tsbox::ts_xts() |>
  ytd_gr() |>
  tail()
#>               VISNS_HI
#> 2024-10-01 -0.40247385
#> 2024-11-01  0.07062139
#> 2024-12-01  0.55726082
#> 2025-01-01  3.75872518
#> 2025-02-01  1.03493481
#> 2025-03-01  1.73742680
```
