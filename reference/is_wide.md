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

returns TRUE for wide format data frame (time and value columns), FALSE
otherwise

## Examples

``` r
monthly_data_example |> is_wide()
#> [1] TRUE
monthly_data_example |>
  tsbox::ts_long() |>
  is_wide()
#> [1] FALSE
dat_in <- monthly_data_example |>
  tsbox::ts_long() |>
  tsbox::ts_tslist()
wide_df <- is_wide(dat_in)
x_mod <- if (wide_df) tsbox::ts_long(dat_in) else tsbox::ts_tbl(dat_in)
ans <- if (wide_df) tsbox::ts_wide(x_mod) else tsbox::copy_class(x_mod, dat_in)
```
