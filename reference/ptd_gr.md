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

## Details

The growth rate is the percent change of the period-to-date average from
the same date `lag_length` earlier. Weeks start on Sunday. An NA makes
the rest of its period NA.

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
# with per = "week", compare with 52 weeks back so the same weekdays line
# up; "1 year" back lands on a different weekday
daily_data_example |>
  ptd_gr("week", "52 weeks") |>
  tail()
#> # A tibble: 6 × 3
#>   time       VISPNS_HI VAPNS_HI
#>   <date>         <dbl>    <dbl>
#> 1 2022-03-20      47.1     69.1
#> 2 2022-03-21      54.9     75.1
#> 3 2022-03-22      57.7     77.9
#> 4 2022-03-23      54.9     74.6
#> 5 2022-03-24      51.7     70.1
#> 6 2022-03-25      49.0     67.7
# and use lag_length = "364 days" with daily data
daily_data_example |>
  ptd_gr("day", "364 days") |>
  tail()
#> # A tibble: 6 × 3
#>   time       VISPNS_HI VAPNS_HI
#>   <date>         <dbl>    <dbl>
#> 1 2022-03-20      47.1     69.1
#> 2 2022-03-21      63.3     81.7
#> 3 2022-03-22      64.4     84.6
#> 4 2022-03-23      47.5     64.8
#> 5 2022-03-24      42.1     54.2
#> 6 2022-03-25      37.9     57.1
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
