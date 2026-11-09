# Convert "ts-boxable" objects to xts format (extension of `tsbox::ts_xts()`)

Convert "ts-boxable" objects to xts format (extension of
[`tsbox::ts_xts()`](https://docs.ropensci.org/tsbox/reference/ts_ts.html))

## Usage

``` r
conv_xts(x)
```

## Arguments

- x:

  a "ts-boxable" object to be converted

## Value

returns an object in xts format.

## Details

This function performs a similar operation to
[`tsbox::ts_xts()`](https://docs.ropensci.org/tsbox/reference/ts_ts.html).
It converts ts-boxable objects to the xts format. An important
difference compared with
[`tsbox::ts_xts()`](https://docs.ropensci.org/tsbox/reference/ts_ts.html)
is that the x argument of `conv_xts()` can be a wide tbl.

## Examples

``` r
quarterly_data_example |>
  conv_tslist() |>
  conv_xts() |>
  tsbox::ts_pick(1:3) |>
  tail(3)
#>             E_NF_HI   ECT_HI   EMN_HI
#> 2024-07-01 639.7316 39.56811 12.83472
#> 2024-10-01 643.2565 39.11035 12.97680
#> 2025-01-01 647.9022 39.39811 13.16324
quarterly_data_example |>
  conv_wide() |>
  conv_xts() |>
  tsbox::ts_pick(1:3) |>
  tail(3)
#>             E_NF_HI   ECT_HI   EMN_HI
#> 2024-07-01 639.7316 39.56811 12.83472
#> 2024-10-01 643.2565 39.11035 12.97680
#> 2025-01-01 647.9022 39.39811 13.16324
quarterly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_pick("E_NF_HI") |>
  conv_wide() |>
  conv_xts()
#>             E_NF_HI
#> 1998-01-01 530.5066
#> 1998-04-01 531.2062
#> 1998-07-01 532.6954
#> 1998-10-01 531.4825
#> 1999-01-01 529.5065
#> 1999-04-01 533.2071
#> 1999-07-01 537.3354
#> 1999-10-01 541.2599
#> 2000-01-01 544.4418
#> 2000-04-01 551.0984
#>        ...         
#> 2022-10-01 627.5995
#> 2023-01-01 631.2647
#> 2023-04-01 633.7569
#> 2023-07-01 632.2179
#> 2023-10-01 631.8120
#> 2024-01-01 636.9010
#> 2024-04-01 636.4715
#> 2024-07-01 639.7316
#> 2024-10-01 643.2565
#> 2025-01-01 647.9022
```
