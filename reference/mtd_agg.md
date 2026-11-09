# Month to date sum or average

Month to date sum or average

## Usage

``` r
mtd_agg(x, avg = TRUE)
```

## Arguments

- x:

  a ts-boxable object

- avg:

  if TRUE (default), return month to date average, if FALSE, return
  month to date sum

## Value

object of the same type as the input containing month to date sum or
average

## Details

An NA makes the rest of its period NA.

## Examples

``` r
daily_data_example |>
  mtd_agg() |>
  tail()
#> # A tibble: 6 × 3
#>   time       VISPNS_HI VAPNS_HI
#>   <date>         <dbl>    <dbl>
#> 1 2022-03-20    22785.     27.5
#> 2 2022-03-21    22768.     27.7
#> 3 2022-03-22    22624.     27.7
#> 4 2022-03-23    22627.     27.7
#> 5 2022-03-24    22754.     27.8
#> 6 2022-03-25    22866.     28.0
```
