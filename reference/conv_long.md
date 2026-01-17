# Convert "ts-boxable" objects to long format (extension of `tsbox::ts_long()`)

Convert "ts-boxable" objects to long format (extension of
[`tsbox::ts_long()`](https://docs.ropensci.org/tsbox/reference/ts_long.html))

## Usage

``` r
conv_long(x, ser_info = FALSE)
```

## Arguments

- x:

  a "ts-boxable" object to be converted

- ser_info:

  should additional details be returned (TRUE) or only the long format
  of x (default: FALSE)

## Value

returns a ts-boxable object in long format with `id`, `time` and `value`
columns. if `ser_info = TRUE`, also returns the following attributes:
`was_wide` is `TRUE` if x is a wide data frame, `FALSE` otherwise, and
`ser_names` are the names of the series in x.

## Details

This function performs a similar operation to
[`tsbox::ts_long()`](https://docs.ropensci.org/tsbox/reference/ts_long.html).
It converts wide data frames and other ts-boxable objects to the long
format (wide data frames are not ts-boxable). An important difference
compared with
[`tsbox::ts_long()`](https://docs.ropensci.org/tsbox/reference/ts_long.html)
is that `conv_long()` ensures that objects containing a single time
series get an id column.

## Examples

``` r
quarterly_data_example |>
  conv_long()
#> # A tibble: 3,270 × 3
#>    id      time       value
#>    <chr>   <date>     <dbl>
#>  1 E_NF_HI 1998-01-01  531.
#>  2 E_NF_HI 1998-04-01  531.
#>  3 E_NF_HI 1998-07-01  533.
#>  4 E_NF_HI 1998-10-01  531.
#>  5 E_NF_HI 1999-01-01  530.
#>  6 E_NF_HI 1999-04-01  533.
#>  7 E_NF_HI 1999-07-01  537.
#>  8 E_NF_HI 1999-10-01  541.
#>  9 E_NF_HI 2000-01-01  544.
#> 10 E_NF_HI 2000-04-01  551.
#> # ℹ 3,260 more rows
quarterly_data_example |>
  conv_long() |>
  tsbox::ts_tslist() |>
  conv_long()
#> # A tibble: 3,270 × 3
#>    id      time       value
#>    <chr>   <date>     <dbl>
#>  1 E_NF_HI 1998-01-01  531.
#>  2 E_NF_HI 1998-04-01  531.
#>  3 E_NF_HI 1998-07-01  533.
#>  4 E_NF_HI 1998-10-01  531.
#>  5 E_NF_HI 1999-01-01  530.
#>  6 E_NF_HI 1999-04-01  533.
#>  7 E_NF_HI 1999-07-01  537.
#>  8 E_NF_HI 1999-10-01  541.
#>  9 E_NF_HI 2000-01-01  544.
#> 10 E_NF_HI 2000-04-01  551.
#> # ℹ 3,260 more rows
quarterly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_xts() |>
  conv_long(ser_info = TRUE)
#> # A tibble: 3,270 × 3
#>    id      time       value
#>    <chr>   <date>     <dbl>
#>  1 E_NF_HI 1998-01-01  531.
#>  2 E_NF_HI 1998-04-01  531.
#>  3 E_NF_HI 1998-07-01  533.
#>  4 E_NF_HI 1998-10-01  531.
#>  5 E_NF_HI 1999-01-01  530.
#>  6 E_NF_HI 1999-04-01  533.
#>  7 E_NF_HI 1999-07-01  537.
#>  8 E_NF_HI 1999-10-01  541.
#>  9 E_NF_HI 2000-01-01  544.
#> 10 E_NF_HI 2000-04-01  551.
#> # ℹ 3,260 more rows
quarterly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_pick("E_NF_HI") |>
  tsbox::ts_xts() |>
  conv_long()
#> # A tibble: 109 × 3
#>    id      time       value
#>    <chr>   <date>     <dbl>
#>  1 E_NF_HI 1998-01-01  531.
#>  2 E_NF_HI 1998-04-01  531.
#>  3 E_NF_HI 1998-07-01  533.
#>  4 E_NF_HI 1998-10-01  531.
#>  5 E_NF_HI 1999-01-01  530.
#>  6 E_NF_HI 1999-04-01  533.
#>  7 E_NF_HI 1999-07-01  537.
#>  8 E_NF_HI 1999-10-01  541.
#>  9 E_NF_HI 2000-01-01  544.
#> 10 E_NF_HI 2000-04-01  551.
#> # ℹ 99 more rows
quarterly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_xts() |>
  tsbox::ts_pick("E_NF_HI") |>
  conv_long()
#> # A tibble: 109 × 3
#>    id      time       value
#>    <chr>   <date>     <dbl>
#>  1 E_NF_HI 1998-01-01  531.
#>  2 E_NF_HI 1998-04-01  531.
#>  3 E_NF_HI 1998-07-01  533.
#>  4 E_NF_HI 1998-10-01  531.
#>  5 E_NF_HI 1999-01-01  530.
#>  6 E_NF_HI 1999-04-01  533.
#>  7 E_NF_HI 1999-07-01  537.
#>  8 E_NF_HI 1999-10-01  541.
#>  9 E_NF_HI 2000-01-01  544.
#> 10 E_NF_HI 2000-04-01  551.
#> # ℹ 99 more rows
```
