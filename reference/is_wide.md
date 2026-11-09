# Check if a data frame is in wide format

Check if a data frame is in wide format

## Usage

``` r
is_wide(x)
```

## Arguments

- x:

  tibble or data frame

## Value

TRUE for a data frame with a `time` column and numeric value columns
only, FALSE for a long data frame or any object that is not a data
frame. A data frame without a `time` column, or without a numeric
column, stops with an error.

## Examples

``` r
monthly_data_example |> is_wide()
#> [1] TRUE
monthly_data_example |>
  tsbox::ts_long() |>
  is_wide()
#> [1] FALSE
monthly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_tslist() |>
  is_wide()
#> [1] FALSE
```
