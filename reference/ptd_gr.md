# Period to date growth rate

Period to date growth rate

## Usage

``` r
ptd_gr(x, per = "year", lag_length = "1 year")
```

## Arguments

- x:

  a ts-boxable object

- per:

  period over which the sum or average of a higher frequency series in x
  is calculated (this is the unit of time at which the aggregation is
  performed: for ytd per = "year" (default), for mtd per = "month")

- lag_length:

  lag over which growth is calculated or time difference relative to
  base period (e.g. "1 year" (default), "3 years", etc. See ?ts_lag()
  for options)

## Value

object of the same type as the input containing period to date growth
rate

## Examples

``` r
monthly_data_example |>
  ptd_gr() |>
  tail()
#> # A tibble: 6 × 4
#>   time       VISNS_HI VAPNS_HI VADCNS_HI
#>   <date>        <dbl>    <dbl>     <dbl>
#> 1 2024-10-01  -0.402      1.06     -3.46
#> 2 2024-11-01   0.0706     1.23     -3.04
#> 3 2024-12-01   0.557      1.63     -2.54
#> 4 2025-01-01   3.76       3.86      1.90
#> 5 2025-02-01   1.03       1.94      1.89
#> 6 2025-03-01   1.74       2.15      2.00
monthly_data_example |>
  dplyr::select("time", "VAPNS_HI") |>
  ptd_gr(per = "month", lag_length = "3 years") |>
  tail()
#> # A tibble: 6 × 2
#>   time       VAPNS_HI
#>   <date>        <dbl>
#> 1 2024-10-01     37.3
#> 2 2024-11-01     21.4
#> 3 2024-12-01     22.4
#> 4 2025-01-01     36.0
#> 5 2025-02-01     22.0
#> 6 2025-03-01     13.2
# don't use lag_length = "1 year" with weekly data
daily_data_example |>
  ptd_gr("week")
#> # A tibble: 206 × 3
#>    time       VISPNS_HI VAPNS_HI
#>    <date>         <dbl>    <dbl>
#>  1 2021-09-01     1848.     657.
#>  2 2021-09-02     2200.     772.
#>  3 2021-09-03     2280.     830.
#>  4 2021-09-04     2330.     947.
#>  5 2021-09-05     1946.     863.
#>  6 2021-09-06     2696.    1217.
#>  7 2021-09-07     2147.    1043.
#>  8 2021-09-08     2137.    1064.
#>  9 2021-09-09     2287.    1076.
#> 10 2021-09-10     2242.    1055.
#> # ℹ 196 more rows
# lag_length = "52 weeks" instead
daily_data_example |>
  ptd_gr("week", "52 weeks")
#> # A tibble: 207 × 3
#>    time       VISPNS_HI VAPNS_HI
#>    <date>         <dbl>    <dbl>
#>  1 2021-08-31     1558.     601.
#>  2 2021-09-01     1999.     720.
#>  3 2021-09-02     2131.     794.
#>  4 2021-09-03     2244.     925.
#>  5 2021-09-04     2358.     986.
#>  6 2021-09-05     2898.    1220.
#>  7 2021-09-06     2166.    1045.
#>  8 2021-09-07     2096.    1077.
#>  9 2021-09-08     2195.    1062.
#> 10 2021-09-09     2180.    1043.
#> # ℹ 197 more rows
# and use lag_length = "364 days" with daily data
daily_data_example |>
  ptd_gr("day", "364 days")
#> # A tibble: 207 × 3
#>    time       VISPNS_HI VAPNS_HI
#>    <date>         <dbl>    <dbl>
#>  1 2021-08-31     1536.     572.
#>  2 2021-09-01     3190      995.
#>  3 2021-09-02     2741.    1063.
#>  4 2021-09-03     2844.    1663.
#>  5 2021-09-04     3025.    1342.
#>  6 2021-09-05     2898.    1220.
#>  7 2021-09-06     1666.     910.
#>  8 2021-09-07     1967.    1147.
#>  9 2021-09-08     2526.    1016.
#> 10 2021-09-09     2132.     979.
#> # ℹ 197 more rows
daily_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_pick("VAPNS_HI") |>
  ptd_gr("week", "4 weeks") %>%
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
