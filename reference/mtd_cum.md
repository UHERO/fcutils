# Month to date sum or average

Month to date sum or average

## Usage

``` r
mtd_cum(x, avg = TRUE)
```

## Arguments

- x:

  a ts-boxable object

- avg:

  if TRUE (default), return month to date average, if FALSE, return
  month to date sum

## Value

object of the same type as the input containing year to date sum or
average

## Examples

``` r
daily_data_example |>
  mtd_cum()
#> # A tibble: 571 × 3
#>    time       VISPNS_HI VAPNS_HI
#>    <date>         <dbl>    <dbl>
#>  1 2020-09-01      783      2.52
#>  2 2020-09-02      726.     2.32
#>  3 2020-09-03      749      2.27
#>  4 2020-09-04      760.     2.06
#>  5 2020-09-05      752.     1.98
#>  6 2020-09-06      712      1.89
#>  7 2020-09-07      718.     1.89
#>  8 2020-09-08      713.     1.84
#>  9 2020-09-09      698.     1.82
#> 10 2020-09-10      706.     1.82
#> # ℹ 561 more rows
test <- daily_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_pick("VAPNS_HI") |>
  mtd_cum()
tsbox::`%ts/%`(test, tsbox::ts_lag(test, "6 months")) |> tail()
#> # A tibble: 6 × 3
#>   id       time       value
#>   <chr>    <date>     <dbl>
#> 1 VAPNS_HI 2022-03-20  1.35
#> 2 VAPNS_HI 2022-03-21  1.36
#> 3 VAPNS_HI 2022-03-22  1.37
#> 4 VAPNS_HI 2022-03-23  1.37
#> 5 VAPNS_HI 2022-03-24  1.38
#> 6 VAPNS_HI 2022-03-25  1.38
```
