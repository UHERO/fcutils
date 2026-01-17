# Month to date growth rate

Month to date growth rate

## Usage

``` r
mtd_gr(x)
```

## Arguments

- x:

  a ts-boxable object

## Value

object of the same type as the input containing month to date growth
rate

## Examples

``` r
daily_data_example |>
  mtd_gr() |>
  tail()
#> # A tibble: 6 × 3
#>   time       VISPNS_HI VAPNS_HI
#>   <date>         <dbl>    <dbl>
#> 1 2022-03-20      91.1     90.3
#> 2 2022-03-21      88.7     88.6
#> 3 2022-03-22      86.3     87.4
#> 4 2022-03-23      86.5     87.5
#> 5 2022-03-24      85.5     86.6
#> 6 2022-03-25      82.9     85.4
```
