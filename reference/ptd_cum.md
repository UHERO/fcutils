# Period to date sum or average

Period to date sum or average

## Usage

``` r
ptd_cum(x, per = "year", avg = TRUE)
```

## Arguments

- x:

  a ts-boxable object

- per:

  period over which the sum or average of a higher frequency series in x
  is calculated (this is the unit of time at which the aggregation is
  performed: for ytd per = "year" (default), for mtd per = "month")

- avg:

  if TRUE (default), retorn period to date average, if FALSE, return
  period to date sum

## Value

object of the same type as the input containing period to date sum or
average

## Examples

``` r
daily_data_example |>
  ptd_cum("week")
#> # A tibble: 571 × 3
#>    time       VISPNS_HI VAPNS_HI
#>    <date>         <dbl>    <dbl>
#>  1 2020-09-01      783      2.52
#>  2 2020-09-02      726.     2.32
#>  3 2020-09-03      749      2.27
#>  4 2020-09-04      760.     2.06
#>  5 2020-09-05      752.     1.98
#>  6 2020-09-06      513      1.44
#>  7 2020-09-07      633      1.66
#>  8 2020-09-08      648.     1.61
#>  9 2020-09-09      631.     1.62
#> 10 2020-09-10      661.     1.66
#> # ℹ 561 more rows
test <- daily_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_pick("VAPNS_HI") |>
  ptd_cum("week")
tsbox::`%ts/%`(test, tsbox::ts_lag(test, "4 weeks")) |>
  tsbox::`%ts-%`(1) |>
  tsbox::`%ts*%`(100) |>
  tail()
#> # A tibble: 6 × 3
#>   id       time       value
#>   <chr>    <date>     <dbl>
#> 1 VAPNS_HI 2022-03-20  8.55
#> 2 VAPNS_HI 2022-03-21 10.5 
#> 3 VAPNS_HI 2022-03-22 10.7 
#> 4 VAPNS_HI 2022-03-23 13.6 
#> 5 VAPNS_HI 2022-03-24 15.4 
#> 6 VAPNS_HI 2022-03-25 17.2 
```
