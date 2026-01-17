# Year to date sum or average

Year to date sum or average

## Usage

``` r
ytd_cum(x, avg = TRUE)
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

## Examples

``` r
monthly_data_example |>
  ytd_cum()
#> # A tibble: 295 × 4
#>    time       VISNS_HI VAPNS_HI VADCNS_HI
#>    <date>        <dbl>    <dbl>     <dbl>
#>  1 2000-09-01     539.     546.      146.
#>  2 2000-10-01     553.     575.      149.
#>  3 2000-11-01     547.     576.      151.
#>  4 2000-12-01     556.     586.      158.
#>  5 2001-01-01     558.     578.      182.
#>  6 2001-02-01     556.     569.      179.
#>  7 2001-03-01     577.     591.      177.
#>  8 2001-04-01     572.     592.      171.
#>  9 2001-05-01     563.     591.      165.
#> 10 2001-06-01     569.     604.      168.
#> # ℹ 285 more rows
monthly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_pick("VISNS_HI") |>
  tsbox::ts_xts() |>
  ytd_cum(avg = FALSE) |>
  tsbox::ts_plot()
```
