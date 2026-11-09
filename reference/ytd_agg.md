# Year to date sum or average

Year to date sum or average

## Usage

``` r
ytd_agg(x, avg = TRUE)
```

## Arguments

- x:

  a ts-boxable object

- avg:

  if TRUE (default), return year to date average, if FALSE, return year
  to date sum

## Value

object of the same type as the input containing year to date sum or
average

## Details

An NA makes the rest of its period NA.

## Examples

``` r
monthly_data_example |>
  ytd_agg() |>
  tail()
#> # A tibble: 6 × 4
#>   time       VISNS_HI VAPNS_HI VADCNS_HI
#>   <date>        <dbl>    <dbl>     <dbl>
#> 1 2024-10-01     803.     927.      227.
#> 2 2024-11-01     800.     922.      226.
#> 3 2024-12-01     809.     930.      229.
#> 4 2025-01-01     792.     900.      245.
#> 5 2025-02-01     776.     862.      241.
#> 6 2025-03-01     819.     905.      242.
monthly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_pick("VISNS_HI") |>
  tsbox::ts_xts() |>
  ytd_agg(avg = FALSE) |>
  tsbox::ts_plot()
```
